{-# LANGUAGE BangPatterns #-}  

module Dice ( gofast ) where

import Control.Monad
import Data.List
import System.Random
import System.IO.Unsafe

gofast :: Int -> Int -> Int
gofast !casas !vezes = foldl' (+) 0 $ unsafePerformIO $ forM [1..vezes] $ \_ ->  (randomRIO (1,casas) :: IO Int)
