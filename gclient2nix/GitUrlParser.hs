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
  , googleRev :: Text
  } deriving Show

type Parser = Parsec Void Text

parseGitRef :: Parser GitRef
parseGitRef = try parseGoogleSourceRef <|> try parseGithubRef <|> parsePlainGitRef

parseGoogleSourceRef :: Parser GitRef
parseGoogleSourceRef = do
  _ <- string "https://chromium.googlesource.com/"
  fragment <- pack <$> some (alphaNumChar <|> oneOf ['/','.','-'])
  _ <- char '@'
  gitrev <- pack <$> some (alphaNumChar <|> char '.')
  eof
  pure $ GoogleSourceRef ("https://chromium.googlesource.com/" <> fragment) gitrev

parseGithubRef :: Parser GitRef
parseGithubRef = do
  _ <- string "https://github.com/"
  owner' <- pack <$> some alphaNumChar
  _ <- char '/'
  repo' <- pack <$> some (alphaNumChar <|> char '.')
  _ <- char '@'
  gitrev <- pack <$> some (alphaNumChar <|> char '.')
  eof
  pure $ GitHubRef owner' repo' gitrev

parsePlainGitRef :: Parser GitRef
parsePlainGitRef = do
  url' <- pack <$> some (alphaNumChar <|> char '.' <|> char ':' <|> char '/' <|> char '-')
  _ <- char '@'
  gitrev <- pack <$> some (alphaNumChar <|> char '.')
  eof
  pure $ GitRef url' gitrev
