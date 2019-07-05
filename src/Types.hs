{-# LANGUAGE OverloadedStrings #-}
module Types where

import Data.Aeson
import Data.Aeson.Types
import GHC.Generics

import Control.Monad

data Item = Story | Comment | Other
  deriving (Show, Eq)

data Story = MkStory
           { sId    :: Int
           , sTitle :: String
           , sKids  :: Maybe [Int]
           }
  deriving (Show, Eq)

instance FromJSON Story where
  parseJSON = parseStory

data Comment = MkComment
             { cId   :: Int
             , cBy   :: String
             , cKids :: Maybe [Int]
             }
  deriving (Show, Eq)

instance FromJSON Comment where
  parseJSON = parseComment

parseStory :: Value -> Parser Story
parseStory = withObject "Story" $ \o -> do
  _sId    <- o .: "id"
  _sTitle <- o .: "title"
  _sKids  <- o .:? "kids"
  return $ MkStory _sId _sTitle _sKids

parseComment :: Value -> Parser Comment
parseComment = withObject "Story" $ \o -> do
  _cId    <- o .: "id"
  _cBy    <- o .: "by"
  _cKids  <- o .:? "kids"
  return $ MkComment _cId _cBy _cKids




