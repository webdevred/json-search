
{-# LANGUAGE ImportQualifiedPost #-}
module Query (Query(..),advancedQuery) where

import Relude.Unsafe (fromJust)
import Relude hiding (ByteString)
import qualified Data.Attoparsec.ByteString as A
import qualified Data.Attoparsec.ByteString.Char8 as C
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS (null)
import Data.Attoparsec.Internal.Types(Parser)

import Control.Applicative ((<|>))

import Data.ByteString.Internal (c2w)
import qualified Data.ByteString.Lazy as BL

import Control.Monad
import qualified Data.Map as Map

import Data.Vector ((!?))

import MapForest

import Data.Text qualified as T

data Query
  = AdvancedQuery Text
  | SimpleQuery Text
  deriving (Show)

keySelector :: Parser ByteString Selector
keySelector = do
  _ <- C.char '.'
  treeKey <- A.takeWhile (\e -> e `notElem` [c2w a | a <- ['[', '.', ' ']])
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
  -> Maybe MapForest
andEither (Right _) (Right expr) = Just expr
andEither (Left _) _             = Nothing
andEither _ (Left _)             = Nothing

orEither ::
     Either Selector MapForest
  -> Either Selector MapForest
  -> Maybe MapForest
orEither _ (Right expr)          = Just expr
orEither _ (Left _)              = Nothing

justToRight :: Maybe MapForest -> Either Selector MapForest 
justToRight = Right . fromJust

runExpr :: Expr -> MapForest -> Either Selector MapForest
runExpr (And expr1 expr2) mf = justToRight $ andEither (runExpr expr1 mf) (runExpr expr2 mf)
runExpr (Or expr1 expr2) mf = justToRight $ orEither (runExpr expr1 mf) (runExpr expr2 mf)
runExpr (Getter expr) mf = runGetter expr mf

getterParser :: Parser ByteString Getter
getterParser = C.many' (keySelector <|> indexSelector)

data Expr
  = And Expr Expr
  | Or Expr Expr
  | Getter Getter deriving Show

orExprParser :: Parser ByteString Expr
orExprParser = do
  _ <- A.string "or"
  _ <- C.char ' '
  expr1 <- exprParser
  _ <- C.char ' '
  Or expr1 <$> exprParser

andExprParser :: Parser ByteString Expr
andExprParser = do
  _ <- A.string "and"
  _ <- C.char ' '
  expr1 <- exprParser
  _ <- C.char ' '
  And expr1 <$> exprParser

getterExprParser :: Parser ByteString Expr
getterExprParser = Getter <$> getterParser

exprParser :: Parser ByteString Expr
exprParser = orExprParser <|> andExprParser <|> getterExprParser

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
runGetter funs = foldl' (>=>) id (map select funs) . Right
