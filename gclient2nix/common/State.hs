{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module State (HashCache(HashCache, hcCache), Dep(..), DepsInfo(..), DepType(..)) where

import Data.Aeson
import Data.Aeson.Types (Parser, typeMismatch)
import GHC.Generics
import Data.Text (Text)
import qualified Data.HashMap.Strict as HMS
import GitUrlParser (GitRef, parseGitRef)
import Data.Maybe
import qualified Data.Text as T
import Text.Megaparsec
import Data.Void

data HashCache = HashCache
  { hcCache :: HMS.HashMap Text Text
  } deriving (Show, Generic)

instance ToJSON HashCache where
  toJSON = genericToJSON defaultOptions

instance FromJSON HashCache where
  parseJSON = genericParseJSON defaultOptions

data DepsInfo = DepsInfo
  { vars :: HMS.HashMap Text Value
  , deps :: HMS.HashMap Text Dep
  , recursedeps :: [Text]
  } deriving Show

instance FromJSON DepsInfo where
  parseJSON info = do
    let
      parseDeps :: Value -> Parser (HMS.HashMap Text Dep)
      parseDeps = withObject "DepsInfo.deps" (\o -> mapM parseJSON o)
      parseVars :: Value -> Parser (HMS.HashMap Text Value)
      parseVars = withObject "DepsInfo.vars" (\o -> mapM parseJSON o)
    withObject "DepsInfo" (\v -> DepsInfo <$> (v .: "vars" >>= parseVars) <*> (v .: "deps" >>= parseDeps) <*> ((fromMaybe []) <$> v .:? "recursedeps")) info

data Dep = Dep
  { depUrl :: GitUrlParser.GitRef
  , depType :: DepType
  , condition :: Maybe Text
  } | DepUnhandled deriving Show

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

data DepType = Git deriving Show

instance FromJSON DepType where
  parseJSON input = do
    let
      text2type :: Text -> Parser DepType
      text2type "git" = pure Git
      text2type err = typeMismatch "DepType.1" $ String err
    withText "DepType" text2type input

