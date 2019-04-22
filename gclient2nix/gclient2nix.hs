{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE NamedFieldPuns    #-}

module Main (main) where

import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Data.Text (Text)
import           Data.Aeson (encodeFile, eitherDecodeFileStrict, eitherDecode')
import qualified Data.ByteString.Lazy as LBS
import qualified Data.HashMap.Strict as HMS
import           Data.Fix (Fix(Fix))
import           Data.Char (isSpace)
import           Control.Monad.Managed (liftIO, runManaged)
import           Text.Megaparsec (parse)
import           Data.Foldable (foldlM)
import           Prelude hiding (FilePath)
import           Turtle.Prelude (inproc, strict, mktempdir)
import           Turtle (FilePath, (</>), encodeString, procStrict)
import           GHC.Stack (HasCallStack)
import           Data.IORef (IORef, readIORef, newIORef, writeIORef)
import           System.Directory (doesFileExist)
import           Control.Monad.Extra (mapMaybeM)
import           Data.List (intercalate)
import           System.IO (hPutStrLn, hPrint, stderr)

import           Nix.Expr.Shorthands ((@@), mkNonRecSet, ($=), mkStr, mkBool, mkSelector, mkLets, mkParamset, mkFunction)
import           Nix.Expr.Types (NExpr, NExprF(NSym, NSelect, NStr), Antiquoted(Antiquoted,Plain), NString(Indented))
import           Nix.Pretty (prettyNix)

import           GitUrlParser (GitRef(GitHubRef, GitRef, GoogleSourceRef), parseGitRef, RevRef(RevRefRef, RevRefRev))
import           State (DepsInfo(DepsInfo,recursedeps,deps), HashCache(hcCache,HashCache), Dep(Dep,DepUnhandled,condition,depUrl), DepType(Git))

data PrefetchedDep = PrefetchedDep
  { pdExpr :: NExpr
  , pdStorepath :: Text
  , pdDepsInfo :: Maybe DepsInfo
  } deriving Show


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
    rooturl :: String
    rooturl = "https://github.com/electron/electron@9c3cb55ef296254564d72ff9013813f2b03d03b5"
    dest :: Text
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
        checkCondition :: Maybe Text -> Bool
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
