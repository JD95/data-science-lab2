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
import qualified Data.Vector as V
import qualified Data.Text as T
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad
import Data.Aeson.Encode.Pretty
import Data.Time.Clock

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

messageKeys file = do 
  dt <- decode file :: Maybe Value
  mems <- dt ^? nth 0 . key "message" <&> (^@.. members)
  return $ fmap fst mems

maybeIO ma f = maybe (print "Nothing") f $ ma

loadData :: FromJSON a => String -> IO (Maybe a)
loadData filepath = decode <$> BL.readFile filepath

someFunc :: IO ()
someFunc = do
  employees <- loadData @[Employee] "data/employees.json"
  proxOuts <- loadData @[Value] "data/proxOut-MC2.json"
  print $ fmap (take 5) proxOuts
--  maybeIO (proxOuts) $ mapM_ print
  -- maybeIO (loadEmployees eFile) $ mapM_ print

    
