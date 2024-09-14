{-# LANGUAGE ImportQualifiedPost #-}
module Query (Query(..),advancedQuery) where

import Relude hiding (ByteString)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as C
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (null)
import Data.Attoparsec.Internal.Types(Parser)

import Control.Applicative ((<|>))

import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy as BL


-- import Data.Text (Text)
-- import Control.Applicative ((<|>))

import Control.Monad
import qualified Data.Map as Map

import Data.Vector ((!?))

import MapForest

import Data.Text qualified as T
-- import Text.Read (readMaybe)
-- import Data.Word


data Query = AdvancedQuery Text | SimpleQuery Text deriving Show


keySelector :: Parser ByteString Selector
keySelector = do
  _ <- C.char '.'
  treeKey <- (A.takeWhile (\e -> not $ elem e [c2w a | a <- ['[', '.']]))
  return . TreeKey $ decodeUtf8 treeKey

indexSelector :: Parser ByteString Selector
indexSelector = do
  _ <- C.char '['
  branchIndex <- C.decimal
  _ <- C.char ']'
  return . BranchIndex $ branchIndex

queryError :: Selector -> Text
queryError (BranchIndex i) = T.append "can not find BranchIndex " $ show i
queryError (TreeKey k) = T.append "can not find Treekey " k
  
advancedQuery :: ByteString -> MapForest -> MapForest
advancedQuery predicateStr mapForest = case C.parseOnly getterParser predicateStr of
  Right ps -> case runSelectors (map select ps) mapForest of
                Right mf -> mf
                Left s -> error $ queryError s
  Left _ -> error "invalid selector"

getterParser
  :: Parser ByteString [Selector]
getterParser = do
  selectors <- C.many' (keySelector <|> indexSelector )
  rest <- A.takeByteString
  if BS.null rest then
    return selectors
  else
    fail $ "not whole string matched, invalid: " ++ decodeUtf8 rest

data Selector = BranchIndex Int | TreeKey Text deriving Show

select :: Selector -> MapForest -> Either Selector MapForest
select selector@(TreeKey k) (Tree m) = case Map.lookup k m of
  Just val -> Right val
  Nothing -> Left selector

select selector@(BranchIndex i) (Branch v) =
  case v !? i of
    Just val -> Right val
    Nothing -> Left selector
        
select selector _ = Left selector

runSelectors :: [MapForest -> Either Selector MapForest] -> MapForest -> Either Selector MapForest
runSelectors funs = foldl' (>=>) (id) funs . Right
