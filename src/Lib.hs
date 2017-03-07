{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE RecordWildCards #-}

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

data Employee = Employee { lastName :: String
                         , firstName :: String
                         , department :: String
                         , office :: Int
                         } deriving (Show, Eq, Generic, ToJSON, FromJSON)

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

messageKeys file = do 
  dt <- decode file :: Maybe Value
  mems <- dt ^? nth 0 . key "message" <&> (^@.. members)
  return $ fmap fst mems

maybeIO ma f = maybe (print "Nothing") f $ ma

loadEmployees file = decode file:: Maybe [Employee]

someFunc :: IO ()
someFunc = do
  file <- BL.readFile "data/floor1-MC2.json"
  eFile <- BL.readFile "data/employees.json"
  -- maybeIO (messageKeys file) $ mapM_ print
  maybeIO (loadEmployees eFile) $ mapM_ print
  print "Finished!"



