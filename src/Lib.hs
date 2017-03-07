{-# LANGUAGE OverloadedStrings #-}
module Lib
    ( someFunc
    ) where

import Data.Aeson
import Data.Aeson.Lens
import Control.Lens
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Lazy as HM
import qualified Data.Vector as V
import qualified Data.Text as T
import Control.Monad.Trans.Maybe
import Control.Monad.IO.Class
import Control.Monad

liftMaybe :: (MonadPlus m) => Maybe a -> m a
liftMaybe = maybe mzero return

messageKeys file = do 
  dt <- decode file :: Maybe Value
  mems <- dt ^? nth 0 . key "message" <&> (^@.. members)
  return $ fmap fst mems

someFunc :: IO ()
someFunc = do
  file <- BL.readFile "data/floor1-MC2.json"
  maybe (print "Nothing") (mapM_ print) $ messageKeys file




