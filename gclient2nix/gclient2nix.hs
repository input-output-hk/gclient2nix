{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Main (main, testParse) where

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
import qualified GitUrlParser
import Text.Megaparsec
import Data.Void
import Data.Maybe
import Data.Foldable
import GHC.Stack

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
    withObject "DepsInfo" (\v -> DepsInfo <$> (v .: "vars" >>= parseVars) <*> (v .: "deps" >>= parseDeps) <*> v .: "recursedeps") info

instance FromJSON Dep where
  parseJSON = do
    withObject "Dep" $ \v -> do
      url <- v .: "url"
      let
        eresult :: Either (ParseError Char Void) GitUrlParser.GitRef
        eresult = parse GitUrlParser.parseGitRef "json" url
        getGitRef :: Either (ParseError Char Void) GitUrlParser.GitRef -> Parser GitUrlParser.GitRef
        getGitRef (Right ref) = pure ref
        getGitRef (Left err) = fail $ "while parsing GitUrl(" <> (T.unpack url) <> "): " <> show err
      Dep <$> getGitRef eresult <*> v .: "dep_type" <*> v .:? "condition"

rstrip :: Text -> Text
rstrip = T.reverse . T.dropWhile isSpace . T.reverse

tofu :: Text
tofu = "0000000000000000000000000000000000000000000000000000"

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
gitRefToExpr hash (GitUrlParser.GoogleSourceRef url rev') = (Fix $ NSym "fetchzip") @@ (mkNonRecSet $
  [ "url" $= mkStr (url <> "/+archive/refs/tags/" <> rev' <> ".tar.gz")
  , "stripRoot" $= mkBool False
  , "sha256" $= mkStr hash
  ])

generateDepFetcher :: Dep -> IO NExpr
generateDepFetcher (Dep url Git _condition) = do
  let
    hashless = show $ prettyNix $ gitRefToExpr tofu url
  print hashless
  sha256 <- rstrip <$> (strict $ inproc "nix-universal-prefetch" [ "-E", "with import <nixpkgs> {};" <> (T.pack hashless) ] mempty)
  pure $ gitRefToExpr sha256 url

prefetchDep :: FilePath -> Dep -> IO (Text, NExpr)
prefetchDep tmpdir dep = do
  expr <- generateDepFetcher dep
  print $ prettyNix expr
  storepath <- rstrip <$> (strict $ inproc "nix-build" [ "-o", T.pack $ encodeString $ tmpdir </> "tmproot", "-E", "with import <nixpkgs> {};" <> (T.pack $ show $ prettyNix expr) ] mempty)
  pure (storepath, expr)

data PrefetchedDep = PrefetchedDep
  { pdExpr :: NExpr
  , pdStorepath :: Text
  , pdDepsInfo :: DepsInfo
  } deriving Show

main :: IO ()
main = do
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
          foldOverDeps tmp mempty (dest, root') >>= print

foldOverDeps :: HasCallStack => FilePath -> HMS.HashMap Text PrefetchedDep -> (Text, GitUrlParser.GitRef) -> IO (HMS.HashMap Text PrefetchedDep)
foldOverDeps tmp state (dir, ref) = do
  (path, expr) <- prefetchDep tmp $ Dep ref Git Nothing
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
        state2 = HMS.insert dir (PrefetchedDep expr path info') state
        foldinput = generateFoldInput info'
      print foldinput
      foldlM (foldOverDeps tmp) state2 foldinput

generateFoldInput :: DepsInfo -> [(Text, GitUrlParser.GitRef)]
generateFoldInput DepsInfo{recursedeps,deps} = map extractInfo recursedeps
  where
    extractInfo :: Text -> (Text, GitUrlParser.GitRef)
    extractInfo path = (path, depUrl $ fromJust $ HMS.lookup path deps)

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
