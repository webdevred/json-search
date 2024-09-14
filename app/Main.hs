{-# LANGUAGE ImportQualifiedPost #-}
module Main
  ( main
  ) where

import Relude hiding (ByteString)

import Lib

import MapForest(MapForest(..))

import Data.Aeson (decode)

import qualified Data.ByteString.Lazy as BL

import System.Console.GetOpt

import Data.Text qualified as T

import Data.Maybe (isNothing)

import Query (Query(..))

main :: IO ()
main = do
  contents <- BL.getContents
  args <- getArgs
  parsedArgs <- parseArgs args
  let Options {optQuery = query} = parsedArgs
  when (isNothing query) (BL.putStr "no query provided") 
  case (decode contents :: Maybe MapForest) of
     Just mapForest -> BL.putStr $ manipulateContents (getQuery query) mapForest
     Nothing -> putStrLn "fatal error occurred =( please consider reading the source code =) "

getQuery :: Maybe Query -> Query
getQuery maybeQry = fromMaybe (SimpleQuery "") maybeQry

parseArgs :: [String] -> IO Options
parseArgs argv =
  case getOpt Permute options argv of
    (opts, _, []) -> foldl' (>>=) (return startOptions) opts
    (_, _, errors) -> withFrozenCallStack $ error $ T.pack (intercalate ", " errors)

data Options = Options
  { optQuery :: Maybe Query }

startOptions :: Options
startOptions = Options
  { optQuery = Nothing }


options :: [ OptDescr (Options -> IO Options) ]
options =
    [ Option "s" ["simple-getter"]
        (ReqArg
         (\arg opt -> return opt { optQuery = return (SimpleQuery $ T.pack arg) })
            "SIMPLE GETTER QUERY")
        "Simple Getter",
      Option "a" ["advanced-getter"]
        (ReqArg
         (\arg opt -> return opt { optQuery = return (AdvancedQuery $ T.pack arg) })
            "ADVANCED GETTER")
        "Advanced Getter"
    ]
