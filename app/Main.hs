module Main
  ( main
  ) where

import Relude hiding (ByteString)

import Lib

import Data.Aeson (decode)

import qualified Data.ByteString.Lazy as BL

main :: IO ()
main = do
  contents <- BL.getContents
  args <- getArgs
  case (decode contents :: Maybe MapForest) of
    Just mapForest -> BL.putStr $ manipulateContents (head (fromList args)) mapForest
    Nothing -> putStrLn "fatal error occurred =( please consider reading the source code =) "
