{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}


module CodInfo where

import Data.Maybe (isNothing)
import Data.Text hiding (empty)
import qualified Data.Text as T
-- import qualified Data.ByteString as BS

import Data.Aeson --(FromJSON, ToJSON, (.:), Object(..), parseJSON)
import Data.Aeson.Types (Parser)

import Control.Applicative (empty, (<|>))

data Coords = Coords { cLat :: Float
                     , cLon :: Float
                     } deriving (Show, Eq)

instance FromJSON Coords where
  parseJSON (Object v) = Coords <$>
                         v .: "latitude" <*>
                         v .: "longitude"
  parseJSON _ = empty

instance ToJSON Coords where
  toJSON c = object [ "latitude" .= (cLat c)
                    , "longitude" .= (cLon c) ]

type TextField = Text

data CodInfo = CodInfo { ciName :: TextField
                       , ciWebsite :: TextField
                       , ciEmail :: TextField
                       , ciPhone :: TextField
                       , ciAddress :: Maybe TextField
                       , ciCoords :: Maybe Coords
                       } deriving (Show, Eq)

instance FromJSON CodInfo where
  parseJSON (Object v) = CodInfo <$> v .: "name" <*>
                         v .: "website" <*>
                         v .: "email" <*>
                         v .: "phone" <*>
                         parseAddress <*>
                         parseCoords
    where parseAddress =
            do v1 <- v .:? "address"
               maybe empty checkAddress v1
            where checkAddress :: TextField -> Parser (Maybe TextField)
                  checkAddress a = if T.isInfixOf "NA" a
                                      then return Nothing
                                      else return (Just a)
          parseCoords = v .: "coords" <|> (return Nothing)

  parseJSON _ = empty


instance ToJSON CodInfo where
  toJSON c = object [ "name" .= (ciName c)
                    , "website" .= (ciWebsite c)
                    , "email" .= (ciEmail c)
                    , "phone" .= (ciPhone c)
                    , "address" .= (ciAddress c)
                    , "coords" .= (ciCoords c) ]


isEmptyAddress :: CodInfo -> Bool
isEmptyAddress = isNothing . ciAddress

isEmptyCoords :: CodInfo -> Bool
isEmptyCoords = isNothing . ciCoords
