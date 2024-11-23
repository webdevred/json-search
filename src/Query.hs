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

data Query
  = AdvancedQuery Text
  | SimpleQuery Text
  deriving (Show)

keySelector :: Parser ByteString Selector
keySelector = do
  _ <- C.char '.'
  treeKey <- (A.takeWhile (\e -> not $ elem e [c2w a | a <- ['[', '.', ' ']]))
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
advancedQuery predicateStr mapForest =
  withFrozenCallStack $ case (C.parseOnly (exprParser <* A.endOfInput) predicateStr :: Either String Expr) of
    Right expr ->
      case runExpr expr mapForest of
        Right mf -> mf
        Left s -> error $ queryError s
    Left _ -> error "invalid selector"

andEither ::
     Either Selector MapForest
  -> Either Selector MapForest
  -> Either Selector MapForest
andEither (Right _) (Right expr) = Right expr
andEither (Left expr) _ = Left expr
andEither _ (Left expr) = Left expr

orEither ::
     Either Selector MapForest
  -> Either Selector MapForest
  -> Either Selector MapForest
orEither (Right expr) _ = Right expr
orEither _ (Right expr) = Right expr
orEither _ (Left expr) = Left expr

runExpr :: Expr -> MapForest -> Either Selector MapForest
runExpr (And expr1 expr2) mf = andEither (runExpr expr1 mf) (runExpr expr2 mf)
runExpr (Or expr1 expr2) mf = orEither (runExpr expr1 mf) (runExpr expr2 mf)
runExpr (Getter expr) mf = runGetter expr mf

getterParser :: Parser ByteString Getter
getterParser = do
  selectors <- C.many' (keySelector <|> indexSelector)
  return selectors

data Expr
  = And Expr Expr
  | Or Expr Expr
  | Getter Getter deriving Show

orExprParser = do
  _ <- A.string "or"
  _ <- C.char ' '
  expr1 <- exprParser
  _ <- C.char ' '
  expr2 <- exprParser
  return (Or expr1 expr2)

andExprParser = do
  _ <- A.string "and"
  _ <- C.char ' '
  expr1 <- exprParser
  _ <- C.char ' '
  expr2 <- exprParser
  return (And expr1 expr2)

getterExprParser = do
  expr <- getterParser
  return (Getter expr)

exprParser :: Parser ByteString Expr
exprParser = do
  expr <- orExprParser <|> andExprParser <|> getterExprParser
  return expr

data Selector
  = BranchIndex Int
  | TreeKey Text
  deriving (Show)

type Getter = [Selector]

select :: Selector -> MapForest -> Either Selector MapForest
select selector@(TreeKey k) (Tree m) =
  case Map.lookup k m of
    Just val -> Right val
    Nothing -> Left selector
select selector@(BranchIndex i) (Branch v) =
  case v !? i of
    Just val -> Right val
    Nothing -> Left selector
select selector _ = Left selector

runGetter ::
  Getter
  -> MapForest
  -> Either Selector MapForest
runGetter funs = foldl' (>=>) (id) (map select funs) . Right
