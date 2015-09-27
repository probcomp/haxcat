module Main where

import qualified Data.Map
import qualified Data.Vector
import Data.Random
import Data.RVar (sampleRVar)

import Types
import Haxcat

sampleIO :: RVar a -> IO a
sampleIO = sampleRVar

main :: IO ()
main = do
  sampleIO $ train (Data.Map.fromList [(ColID 0, Data.Vector.fromList [0])]) 2
  putStrLn "done"
