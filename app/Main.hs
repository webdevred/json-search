module Main
  ( main
  ) where

import Lib

import Data.Aeson (decode)

import qualified Data.ByteString.Lazy as BL

import System.Environment (getArgs)

main :: IO ()
main = do
  contents <- BL.getContents
  args <- getArgs
  case (decode contents :: Maybe MapForest) of
    Just mapForest -> BL.putStr $ manipulateContents (head args) mapForest
    Nothing -> putStrLn ("fatal error occurred =( please consider reading the source code =) ")
