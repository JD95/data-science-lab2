{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

module Lib where

import GHC.Generics
import Data.Aeson
import Data.Aeson.Lens
import Data.Aeson.Encode.Pretty
import Data.Time.Clock
import Data.Monoid
import Data.List
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.HashMap.Lazy as HM
import qualified Data.Hashable as H
import qualified Data.Vector as V
import qualified System.Directory as D
import Control.Lens
import Control.Monad
import Control.Arrow
import Control.Applicative

-- Library Modules
import ResourceTypes
import Utilities


loadFloorData :: FilePath -> IO (Maybe (HM.HashMap T.Text (V.Vector Value)))
-- ^ Loads transposed data set from the floor files.
--   Each key holds a vector of all values for that key
--   across the entire file.
loadFloorData path = do
  file <- BL.readFile path
  let extractMessages dt = dt ^.. _Array . each . _Object
  let messages = extractMessages <$> decode @Value file
  return (mergeHashMap <$> messages)

-- Utility for working with IO (Maybe a) values
maybeIO ma f = maybe (print "Nothing") f $ ma

loadData :: FromJSON a => String -> IO (Maybe a)
-- Utility for loading JSON data         
loadData filepath = decode <$> BL.readFile filepath

mergeHashMap :: (Eq k, H.Hashable k) => [HM.HashMap k v] -> HM.HashMap k (V.Vector v)
-- ^ Merges a list of HashMap such that each key
--   contains all the values for each occurance
--   of that key in the original list.
mergeHashMap = foldl' (HM.unionWith (V.++)) mempty . fmap (HM.map (V.singleton))

-- Filters out bad filename characters
validateFileName = T.replace ":" " "
                 . T.replace "\\" " "
                 . T.replace "/" " "

loggingPath :: T.Text -> T.Text
-- ^ Modifies a filepath
loggingPath dir = T.concat ["data-logs/", dir,"/"]

logFloorData :: T.Text -- ^ The directory to store the data logs
             -> T.Text -- ^ The key value to lookup
             -> V.Vector Value -- ^ All the values associated with the key
             -> IO ()
logFloorData dir key dt = do
    D.createDirectoryIfMissing False (T.unpack $ loggingPath dir)
    print $ T.concat ["Logging data to ", path]
    mapM_ (TIO.appendFile (T.unpack path) . format) dt
    where path =  T.concat [loggingPath dir, T.concat [validateFileName key, ".txt"]]
          format v = (T.snoc (v ^. _String) '\n')

-- Loads all floor data into seperate files for each sensor
processFloorData = do
  forM_ ["floor2","floor3"] $ \flr -> do
      let fullPath = ("data/" ++ flr ++ "-MC2.json")
      print $ "Loading data for " ++ fullPath
      dt <- loadFloorData fullPath
      maybeIO (dt) $ HM.foldlWithKey' (\ a k v -> a >> logFloorData (T.pack flr) k v) mempty

-- Loads all of the proxyOut-MC2.json data
proxyOuts = maybe mempty id <$> (loadData @(V.Vector ProxOut) "data/proxOut-MC2.json")

-- Zones
floorZone f z = V.filter (\(ProxOut m _) -> pFloor m == f && zone m ==  z)
deli = floorZone 1 "1"
lgMeeting = floorZone 1 "2"

-- Writes a log of all proxies within a certain area defined by f
logProxInArea f = proxyOuts >>= (writeVectorToFile "data-logs/people-in-lgMeeting.txt" . f)
               >> print "Finished!"

-- Logs the minimal data for the proxy data
logProxyOutData = do
    print "Started Processing Prox Out Data!"
    writeVectorToFile "data-logs/employeeProx.txt" <$> proxyOuts
    print "Finished!"


 

    
