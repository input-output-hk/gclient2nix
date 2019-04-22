{-# LANGUAGE OverloadedStrings #-}

module GitUrlParser where

import Data.Void
import Data.Text
import Text.Megaparsec
import Text.Megaparsec.Char

data GitRef = GitRef
  { url :: Text
  , rev :: Text
  }
  | GitHubRef
  { owner :: Text
  , repo :: Text
  , rev2 :: Text
  }
  | GoogleSourceRef
  { url :: Text
  , googleRev :: RevRef
  } deriving Show

type Parser = Parsec Void Text

data RevRef = RevRefRev { rrRev :: Text } | RevRefRef { rrRef :: Text } deriving Show

parseGitRef :: Parser GitRef
parseGitRef = try parseGoogleSourceRef <|> try parseGithubRef <|> parsePlainGitRef

parseGoogleSourceRef :: Parser GitRef
parseGoogleSourceRef = do
  _ <- string "https://"
  subdomain <- pack <$> some alphaNumChar
  _ <- string ".googlesource.com/"
  fragment <- pack <$> some (alphaNumChar <|> oneOf ['/','.','-', '_'])
  _ <- char '@'
  let
    revOrRef :: Parser RevRef
    revOrRef = try parseRev <|> parseRef
    parseRev :: Parser RevRef
    parseRev = do
      rev' <- some hexDigitChar
      eof
      pure $ RevRefRev $ pack rev'
    parseRef :: Parser RevRef
    parseRef = do
      ref <- some (alphaNumChar <|> char '.')
      eof
      pure $ RevRefRef $ pack ref
  gitrev <- revOrRef
  eof
  pure $ GoogleSourceRef ("https://" <> subdomain <> ".googlesource.com/" <> fragment) gitrev

parseGithubRef :: Parser GitRef
parseGithubRef = do
  _ <- string "https://github.com/"
  owner' <- pack <$> some alphaNumChar
  _ <- char '/'
  repo' <- pack <$> some (alphaNumChar)
  _ <- string ".git"
  _ <- char '@'
  gitrev <- pack <$> some (alphaNumChar <|> char '.')
  eof
  pure $ GitHubRef owner' repo' gitrev

parsePlainGitRef :: Parser GitRef
parsePlainGitRef = do
  url' <- pack <$> some (alphaNumChar <|> oneOf [ '.', ':', '/', '-', '_' ])
  _ <- char '@'
  gitrev <- pack <$> some (alphaNumChar <|> char '.')
  eof
  pure $ GitRef url' gitrev
