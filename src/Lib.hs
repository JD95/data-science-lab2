{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Lib
    ( someFunc
    ) where

import Data.Aeson
import Data.Aeson.Lens
import GHC.Generics
import Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Lazy as HM
import qualified Data.Hashable as H
import qualified Data.Vector as V
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified System.Directory as D
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad
import Data.Aeson.Encode.Pretty
import Data.Time.Clock
import Data.List
import Data.Monoid
import Control.Arrow

data Employee = Employee { lastName :: String
                         , firstName :: String
                         , department :: String
                         , office :: Int
                         } deriving (Show, Eq, Generic, ToJSON, FromJSON)

data ProxMessage = ProxMessage { proxCard :: String
                               , zone :: String
                               , datetime :: String
                               , floor :: String
                               , pType :: String
                               } deriving (Show, Eq, Generic, ToJSON)

instance FromJSON ProxMessage where
    parseJSON = withObject "message" $ \o -> do
        proxCard <- o.:"proxCard" -- earpa001 -> Emil Arpa
        zone <- o.:"zone"
        datetime <- o.:"datetime"
        floor <- o.:"floor"
        pType <- o.:"type"
        return ProxMessage {..}

data ProxOut = ProxOut { message :: ProxMessage
                       , offset :: Double
                       } deriving (Show, Eq, Generic, ToJSON, FromJSON)

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

loadFloorData path = do
  file <- BL.readFile path
  let extractMessages dt = dt ^.. _Array . each . key "message" . _Object
  let messages = extractMessages <$> decode @Value file
  return (fmap mergeHashMap messages)

maybeIO ma f = maybe (print "Nothing") f $ ma

loadData :: FromJSON a => String -> IO (Maybe a)
loadData filepath = decode <$> BL.readFile filepath

mergeHashMap :: (Eq k, H.Hashable k) => [HM.HashMap k v] -> HM.HashMap k [v]
mergeHashMap = foldl' (HM.unionWith (++)) mempty . fmap (HM.map (:[]))

validateFileName = T.replace ":" " "
                 . T.replace "\\" " "
                 . T.replace "/" " "

loggingDir :: T.Text
loggingDir = "data-logs/"

logDataToFile :: T.Text -> [Value] -> IO ()
logDataToFile key dt = do
    print $ T.concat ["Logging data to ", path]
    mapM_ (TIO.appendFile (T.unpack path) . format) dt
    where path =  T.concat [loggingDir, T.concat [validateFileName key, ".txt"]]
          format v = (T.snoc (v ^. _String) '\n')

someFunc :: IO ()
someFunc = do
  floor1 <- loadFloorData "data/floor1-MC2.json"
  employees <- loadData @[Employee] "data/employees.json"
  proxOuts <- loadData @[[ProxOut]] "data/proxOut-MC2.json"
  D.createDirectoryIfMissing False "data-logs"
  -- I need to generalize this to all of the keys
  maybeIO (floor1) $ mapM_ (uncurry logDataToFile)
                   . HM.toList

 

    
