module Utilities where

import Data.Bool
import qualified Data.Vector as V

(?>) :: Ord o => (a -> o) -> (a -> a -> Ordering) -> (a -> a -> Ordering)
f ?> g = \x y -> bool (compare (f x) (f y)) (g x y) (f x /= f y)

writeVectorToFile path v = do
     --D.createDirectoryIfMissing False path
     mapM_ (appendFile path . (++"\n") . show) v
