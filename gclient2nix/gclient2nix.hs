{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Main (main, testParse, testCache) where

--import System.Environment
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.Text (Text)
import Nix.Expr.Shorthands
import Nix.Pretty
import Data.Fix
import Nix.Expr.Types
import Turtle.Prelude (inproc, strict, mktempdir)
import Data.Char (isSpace)
import Control.Monad.Managed (liftIO, runManaged)
import Turtle (FilePath, (</>), encodeString, procStrict)
import Prelude hiding (FilePath)
import Data.Aeson
import qualified Data.ByteString.Lazy as LBS
import Data.Aeson.Types (Parser, typeMismatch)
import qualified Data.HashMap.Strict as HMS
import GitUrlParser (GitRef(GitHubRef, GitRef, GoogleSourceRef), parseGitRef, RevRef(RevRefRef, RevRefRev))
import Text.Megaparsec
import Data.Void
import Data.Maybe
import Data.Foldable
import GHC.Stack
import GHC.Generics
import Data.IORef
import System.Directory
import Control.Monad.Extra
import System.IO (hPutStrLn, hPrint, stderr)
import Data.List

data DepType = Git deriving Show

data DepsInfo = DepsInfo
  { vars :: HMS.HashMap Text Value
  , deps :: HMS.HashMap Text Dep
  , recursedeps :: [Text]
  } deriving Show

data Dep = Dep
  { depUrl :: GitUrlParser.GitRef
  , depType :: DepType
  , condition :: Maybe Text
  } | DepUnhandled deriving Show

data PrefetchedDep = PrefetchedDep
  { pdExpr :: NExpr
  , pdStorepath :: Text
  , pdDepsInfo :: Maybe DepsInfo
  } deriving Show

instance FromJSON DepType where
  parseJSON input = do
    let
      text2type :: Text -> Parser DepType
      text2type "git" = pure Git
      text2type err = typeMismatch "DepType.1" $ String err
    withText "DepType" text2type input

instance FromJSON DepsInfo where
  parseJSON info = do
    let
      parseDeps :: Value -> Parser (HMS.HashMap Text Dep)
      parseDeps = withObject "DepsInfo.deps" (\o -> mapM parseJSON o)
      parseVars :: Value -> Parser (HMS.HashMap Text Value)
      parseVars = withObject "DepsInfo.vars" (\o -> mapM parseJSON o)
    withObject "DepsInfo" (\v -> DepsInfo <$> (v .: "vars" >>= parseVars) <*> (v .: "deps" >>= parseDeps) <*> ((fromMaybe []) <$> v .:? "recursedeps")) info

instance FromJSON Dep where
  parseJSON = do
    withObject "Dep" $ \v -> do
      depType' <- (v .: "dep_type" :: Parser Text)
      case depType' of
        "git" -> do
          url <- v .: "url"
          let
            eresult :: Either (ParseError Char Void) GitUrlParser.GitRef
            eresult = parse GitUrlParser.parseGitRef "json" url
            getGitRef :: Either (ParseError Char Void) GitUrlParser.GitRef -> Parser GitUrlParser.GitRef
            getGitRef (Right ref) = pure ref
            getGitRef (Left err) = fail $ "while parsing GitUrl(" <> (T.unpack url) <> "): " <> show err
          Dep <$> getGitRef eresult <*> v .: "dep_type" <*> v .:? "condition"
        "cipd" -> do
          pure DepUnhandled
        err -> do
          fail $ T.unpack $ "unexpected dep_type: " <> err

rstrip :: Text -> Text
rstrip = T.reverse . T.dropWhile isSpace . T.reverse

tofu :: Text
tofu = "0000000000000000000000000000000000000000000000000000"

getUrl :: GitUrlParser.GitRef -> Text
getUrl (GitUrlParser.GitHubRef owner' repo' rev') = "https://github.com/" <> owner' <> "/" <> repo' <> "/archive/" <> rev' <> ".tar.gz"
getUrl (GitUrlParser.GitRef url' rev') = url' <> "@" <> rev'
getUrl (GitUrlParser.GoogleSourceRef url' (RevRefRef ref')) = url' <> "/+archive/refs/tags/" <> ref' <> ".tar.gz"
getUrl (GitUrlParser.GoogleSourceRef url' (RevRefRev rev')) = url' <> "/+archive/" <> rev' <> ".tar.gz"

gitRefToExpr :: Text -> GitUrlParser.GitRef -> NExpr
gitRefToExpr hash (GitUrlParser.GitHubRef owner' repo' rev') = (Fix $ NSym "fetchFromGitHub") @@ (mkNonRecSet $
  [ "owner" $= mkStr owner'
  , "repo" $= mkStr repo'
  , "rev" $= mkStr rev'
  , "sha256" $= mkStr hash
  ])
gitRefToExpr hash (GitUrlParser.GitRef url' rev') = (Fix $ NSym "fetchgit") @@ (mkNonRecSet $
  [ "url" $= mkStr url'
  , "rev" $= mkStr rev'
  , "sha256" $= mkStr hash
  ])
gitRefToExpr hash url = (Fix $ NSym "fetchzip") @@ (mkNonRecSet $
  [ "url" $= mkStr (getUrl url)
  , "stripRoot" $= mkBool False
  , "sha256" $= mkStr hash
  ])

generateDepFetcher :: IORef HashCache -> Dep -> IO NExpr
generateDepFetcher _ (DepUnhandled) = undefined
generateDepFetcher hashCacheRef (Dep url Git _condition) = do
  hashCache <- readIORef hashCacheRef
  let
    key = getUrl url
    hashGuess = HMS.lookup key (hcCache hashCache)
    finishFetch :: Text -> IO NExpr
    finishFetch sha256 = do
      writeIORef hashCacheRef (HashCache (HMS.insert key sha256 (hcCache hashCache)))
      readIORef hashCacheRef >>= encodeFile "hash-cache.json"
      pure $ gitRefToExpr sha256 url
  case hashGuess of
    Nothing -> do
      let
        hashless = show $ prettyNix $ gitRefToExpr tofu url
      sha256 <- rstrip <$> (strict $ inproc "nix-universal-prefetch" [ "-E", "with import <nixpkgs> {};" <> (T.pack hashless) ] mempty)
      finishFetch sha256
    Just hash -> do
      finishFetch hash

prefetchDep :: (IORef HashCache, FilePath) -> Dep -> IO (Text, NExpr)
prefetchDep (hashCacheRef, tmpdir) dep = do
  expr <- generateDepFetcher hashCacheRef dep
  storepath <- rstrip <$> (strict $ inproc "nix-build" [ "-o", T.pack $ encodeString $ tmpdir </> "tmproot", "-E", "with import <nixpkgs> {};" <> (T.pack $ show $ prettyNix expr) ] mempty)
  pure (storepath, expr)

loadCache :: String -> IO HashCache
loadCache path = do
  exists <- doesFileExist path
  if exists then do
      eitherCache <- eitherDecodeFileStrict path
      case eitherCache of
        Left err -> do
          print err
          pure $ HashCache mempty
        Right cache -> pure cache
    else pure $ HashCache mempty

main :: IO ()
main = do
  hashCache <- loadCache "hash-cache.json"
  ref <- newIORef hashCache
  --[ rooturl ] <- getArgs
  let
    rooturl = "https://github.com/electron/electron@9c3cb55ef296254564d72ff9013813f2b03d03b5"
    dest = "src/electron"
  runManaged $ do
    tmp <- mktempdir "/tmp" "gclient2nix"
    liftIO $ do
      let
        eresult = parse GitUrlParser.parseGitRef "argv[1]" (T.pack rooturl)
      case eresult of
        Left err -> print err
        Right root' -> do
          result <- foldOverDeps (ref,tmp) mempty (dest, True, root')
          let
            srcSet = mkNonRecSet (map (\(key, PrefetchedDep{pdExpr}) -> "\"" <> key <> "\"" $= pdExpr) (HMS.toList result))
          let
            mkRef :: Text -> NExpr
            mkRef path = Fix $ NSelect (Fix $ NSym "sources") (mkSelector $ "\"" <> path <> "\"") Nothing
            copyItem :: (Text, PrefetchedDep) -> [ Antiquoted Text NExpr ]
            copyItem (path, _) =
              [ Plain $ "mkdir -pv $out/" <> path <> "\n"
              , Plain $ "cp --no-preserve=mode,ownership -r "
              , Antiquoted $ mkRef path
              , Plain $ "/* $out/" <> path ]
            createMergedResult :: [ Antiquoted Text NExpr ]
            createMergedResult = (intercalate [ Plain "\n" ] (map copyItem (HMS.toList result))) <>
              [ Plain "\ncd $out\n"
              , Plain "find -type f | sort > listing.txt"
              ]
            merger = (Fix $ NSym "runCommand") @@ mkStr "name" @@ (mkNonRecSet [ "preferLocalBuild" $= mkBool True ]) @@ (Fix $ NStr $ Indented 0 createMergedResult)
            letBlock = mkLets [ "sources" $= srcSet ] merger
            requiredParams = mkParamset [ ("runCommand",Nothing), ("fetchzip",Nothing), ("fetchgit",Nothing), ("fetchFromGitHub", Nothing) ] False
            topLevel = mkFunction requiredParams letBlock
          print $ prettyNix topLevel
          --mapM_ (print) (HMS.toList result)
  readIORef ref >>= encodeFile "hash-cache.json"

foldOverDeps :: HasCallStack => (IORef HashCache,FilePath) -> HMS.HashMap Text PrefetchedDep -> (Text, Bool, GitUrlParser.GitRef) -> IO (HMS.HashMap Text PrefetchedDep)
foldOverDeps other state (dir, recurseInto, ref) = do
  hPrint stderr $ "processing: " <> dir
  (path, expr) <- prefetchDep other $ Dep ref Git Nothing
  case recurseInto of
    True -> do
      (_exitcode, jsonInfo) <- procStrict "gclient2nix" [ path <> "/DEPS" ] mempty
      let
        info :: Either String DepsInfo
        info = eitherDecode' $ LBS.fromStrict $ T.encodeUtf8 jsonInfo
      case info of
        Left err -> do
          print jsonInfo
          print path
          error err
        Right info' -> do
          let
            state2 = HMS.insert dir (PrefetchedDep expr path (Just info')) state
          hPrint stderr $ recursedeps info'
          foldinput <- generateFoldInput info'
          hPutStrLn stderr "fold input:"
          hPrint stderr foldinput
          foldlM (foldOverDeps other) state2 foldinput
    False -> do
      let
        state2 = HMS.insert dir (PrefetchedDep expr path Nothing) state
      pure state2

generateFoldInput :: DepsInfo -> IO [(Text, Bool, GitUrlParser.GitRef)]
generateFoldInput DepsInfo{recursedeps,deps} = mapMaybeM extractInfo (HMS.toList deps)
  where
    extractInfo :: (Text, Dep) -> IO (Maybe (Text, Bool, GitUrlParser.GitRef))
    extractInfo (path, dep@Dep{condition}) = do
      let
        checkCondition (Just "checkout_src_internal") = False
        checkCondition (Just "not build_with_chromium") = False
        checkCondition (Just "checkout_requests") = False
        checkCondition (Just "checkout_boto") = False
        checkCondition Nothing = True
        checkCondition _ = True
      if (checkCondition condition)
        then pure $ Just (path, any (==path) recursedeps, depUrl dep)
        else pure Nothing
    extractInfo (path, DepUnhandled) = do
      hPrint stderr $ "unhandled dep type at " <> path
      pure Nothing

testDepsJson :: Text
testDepsJson = "{\"vars\": {\"electron_git\": \"https://github.com/electron\", \"pyyaml_version\": \"3.12\", \"checkout_node\": true, \"checkout_android\": false, \"boto_git\": \"https://github.com/boto\", \"checkout_requests\": false, \"chromium_version\": \"75.0.3740.3\", \"download_external_binaries\": true, \"yaml_git\": \"https://github.com/yaml\", \"node_version\": \"2dc0f8811b2b295c08d797b8a11b030234c98502\", \"apply_patches\": true, \"requests_git\": \"https://github.com/kennethreitz\", \"checkout_nacl\": false, \"checkout_chromium\": true, \"checkout_oculus_sdk\": false, \"build_with_chromium\": true, \"checkout_android_native_support\": false, \"checkout_libaom\": true, \"requests_version\": \"e4d59bedfd3c7f4f254f4f5d036587bcd8152458\", \"checkout_boto\": false, \"chromium_git\": \"https://chromium.googlesource.com\", \"checkout_pyyaml\": false, \"boto_version\": \"f7574aa6cc2c819430c1f05e9a1a1a666ef8169b\"}, \"hooks\": [{\"action\": [\"python\", \"src/electron/script/apply_all_patches.py\", \"src/electron/patches/common/config.json\"], \"pattern\": \"src/electron\", \"name\": \"patch_chromium\", \"condition\": \"checkout_chromium and apply_patches\"}, {\"action\": [\"python\", \"src/electron/script/update-external-binaries.py\"], \"pattern\": \"src/electron/script/update-external-binaries.py\", \"name\": \"electron_external_binaries\", \"condition\": \"download_external_binaries\"}, {\"action\": [\"python\", \"-c\", \"import os, subprocess; os.chdir(os.path.join(\\\"src\\\", \\\"electron\\\")); subprocess.check_call([\\\"python\\\", \\\"script/lib/npm.py\\\", \\\"install\\\"]);\"], \"pattern\": \"src/electron/package.json\", \"name\": \"electron_npm_deps\"}, {\"action\": [\"python\", \"-c\", \"import os, subprocess; os.chdir(os.path.join(\\\"src\\\", \\\"electron\\\", \\\"vendor\\\", \\\"boto\\\")); subprocess.check_call([\\\"python\\\", \\\"setup.py\\\", \\\"build\\\"]);\"], \"pattern\": \"src/electron\", \"name\": \"setup_boto\", \"condition\": \"checkout_boto\"}, {\"action\": [\"python\", \"-c\", \"import os, subprocess; os.chdir(os.path.join(\\\"src\\\", \\\"electron\\\", \\\"vendor\\\", \\\"requests\\\")); subprocess.check_call([\\\"python\\\", \\\"setup.py\\\", \\\"build\\\"]);\"], \"pattern\": \"src/electron\", \"name\": \"setup_requests\", \"condition\": \"checkout_requests\"}], \"recursedeps\": [\"src\"], \"gclient_gn_args\": [\"build_with_chromium\", \"checkout_android\", \"checkout_android_native_support\", \"checkout_libaom\", \"checkout_nacl\", \"checkout_oculus_sdk\"], \"deps\": {\"src/third_party/electron_node\": {\"url\": \"https://github.com/electron/node.git@2dc0f8811b2b295c08d797b8a11b030234c98502\", \"dep_type\": \"git\", \"condition\": \"checkout_node\"}, \"src\": {\"url\": \"https://chromium.googlesource.com/chromium/src.git@75.0.3740.3\", \"dep_type\": \"git\", \"condition\": \"checkout_chromium\"}, \"src/electron/vendor/boto\": {\"url\": \"https://github.com/boto/boto.git@f7574aa6cc2c819430c1f05e9a1a1a666ef8169b\", \"dep_type\": \"git\", \"condition\": \"checkout_boto\"}, \"src/electron/vendor/requests\": {\"url\": \"https://github.com/kennethreitz/requests.git@e4d59bedfd3c7f4f254f4f5d036587bcd8152458\", \"dep_type\": \"git\", \"condition\": \"checkout_requests\"}, \"src/electron/vendor/pyyaml\": {\"url\": \"https://github.com/yaml/pyyaml.git@3.12\", \"dep_type\": \"git\", \"condition\": \"checkout_pyyaml\"}}, \"gclient_gn_args_file\": \"src/build/config/gclient_args.gni\"}\n"

testParse :: IO ()
testParse = do
  let
    info :: Either String DepsInfo
    info = eitherDecode' $ LBS.fromStrict $ T.encodeUtf8 testDepsJson
  putStrLn $ T.unpack testDepsJson
  print info
  parseTest GitUrlParser.parseGitRef "https://github.com/kennethreitz/requests.git@e4d59bedfd3c7f4f254f4f5d036587bcd8152458"
  parseTest GitUrlParser.parseGitRef "https://github.com/yaml/pyyaml.git@3.12"
  parseTest GitUrlParser.parseGitRef "https://chromium.googlesource.com/chromium/src.git@75.0.3740.3"
  parseTest GitUrlParser.parseGitRef "https://chromium.googlesource.com/external/github.com/google/libprotobuf-mutator.git@439e81f8f4847ec6e2bf11b3aa634a5d8485633d"

data HashCache = HashCache
  { hcCache :: HMS.HashMap Text Text
  } deriving (Show, Generic)

instance ToJSON HashCache where
  toJSON = genericToJSON defaultOptions

instance FromJSON HashCache where
  parseJSON = genericParseJSON defaultOptions

testCache :: IO ()
testCache = do
  let
    cache = HashCache (HMS.insert "key" "value" mempty)
  print $ ((eitherDecode' $ encode cache) :: Either String HashCache)
  pure ()
