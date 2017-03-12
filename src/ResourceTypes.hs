{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-} 
module ResourceTypes ( Employee(..)
                     , ProxMessage(..)
                     , ProxOut(..)
                     ) where

import Data.Aeson
import qualified Data.Text as T
import Data.Time.Clock
import GHC.Generics
import Data.Bool
import Data.Ord
import Data.List

import Utilities

data Employee = Employee { lastName :: T.Text
                         , firstName :: T.Text
                         , department :: T.Text
                         , office :: Int
                         } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ProxMessage = ProxMessage { proxCard :: T.Text
                               , zone :: T.Text
                               , datetime :: UTCTime
                               , pFloor :: Int
                               , pType :: T.Text
                               } deriving (Eq, Generic, ToJSON)

instance Show ProxMessage where
    show m = concat . intersperse "\t" $
        [ T.unpack . T.dropEnd 3 . T.pack $ show (datetime m)
        , T.unpack (proxCard  m)
        , show (pFloor m)
        , T.unpack (zone m)
        ]

instance Ord ProxMessage where
  compare a b = (proxCard ?> comparing datetime) a b


instance FromJSON ProxMessage where
    parseJSON = withObject "message" $ \o -> do
        proxCard <- o.:"proxCard" -- earpa001 -> Emil Arpa
        zone <- o.:"zone"
        datetime <- o.:"datetime"
        floor <- o.:"floor"
        pType <- o.:"type"
        return $ ProxMessage proxCard zone (read @UTCTime datetime) (read @Int floor) pType

data ProxOut = ProxOut { message :: ProxMessage
                       , offset :: Double
                       } deriving (Eq, Generic, ToJSON, FromJSON)

instance Ord ProxOut where
    compare a b = compare (message a) (message b)

instance Show ProxOut where
    show (ProxOut m o) = show m
