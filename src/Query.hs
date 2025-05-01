module Query
  ( Query(..)
  , advancedQuery
  ) where

import Data.Void (Void)
import Text.Megaparsec qualified as MP
import Text.Megaparsec.Byte qualified as B
import Text.Megaparsec.Byte.Lexer qualified as L (decimal)
import Text.Megaparsec.Char qualified as C

import Data.Word (Word8)

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS (null, pack)
import Data.Functor (($>), (<&>))
import Relude hiding (ByteString)
import Relude.Unsafe (fromJust)

import Control.Applicative ((<|>))

import Data.ByteString.Internal (c2w)
import Data.ByteString.Lazy qualified as BL

import Control.Monad
import Data.Map qualified as Map

import Data.Vector ((!?))

import MapForest

import Data.Text qualified as T

type Parser = MP.Parsec Void ByteString

data Query
  = AdvancedQuery Text
  | SimpleQuery Text
  deriving (Show)

tryParsers :: [Parser a] -> Parser a
tryParsers = asum . map MP.try

toChar :: Word8 -> Char
toChar = chr . fromIntegral

byteChar :: Char -> Parser Word8
byteChar = B.char . fromIntegral . ord

keySelector :: Parser Selector
keySelector = byteChar '.' >> packKey
  where
    validKeyChar c = toChar c `notElem` ['.', '[', ' ']
    parseKey = MP.some (MP.satisfy validKeyChar)
    packKey = parseKey <&> TreeKey . decodeUtf8 . BS.pack

indexSelector :: Parser Selector
indexSelector = BranchIndex <$> (byteChar '[' *> L.decimal <* byteChar ']')

queryError :: Selector -> Text
queryError (BranchIndex i) = T.append "can not find BranchIndex " $ show i
queryError (TreeKey k) = T.append "can not find Treekey " k

advancedQuery :: ByteString -> MapForest -> MapForest
advancedQuery predicateStr mapForest =
  withFrozenCallStack
    $ case MP.parse (exprParser <* MP.eof) "<input>" predicateStr of
        Right expr ->
          case runExpr expr mapForest of
            Right mf -> mf
            Left s -> error $ queryError s
        Left _ -> error "invalid selector"

andEither ::
     Either Selector MapForest -> Either Selector MapForest -> Maybe MapForest
andEither (Right _) (Right expr) = Just expr
andEither (Left _) _ = Nothing
andEither _ (Left _) = Nothing

orEither ::
     Either Selector MapForest -> Either Selector MapForest -> Maybe MapForest
orEither _ (Right expr) = Just expr
orEither _ (Left _) = Nothing

justToRight :: Maybe MapForest -> Either Selector MapForest
justToRight = Right . fromJust

runExpr :: Expr -> MapForest -> Either Selector MapForest
runExpr (And expr1 expr2) mf =
  justToRight $ andEither (runExpr expr1 mf) (runExpr expr2 mf)
runExpr (Or expr1 expr2) mf =
  justToRight $ orEither (runExpr expr1 mf) (runExpr expr2 mf)
runExpr (Getter expr) mf = runGetter expr mf

getterParser :: Parser Getter
getterParser = MP.some (tryParsers [keySelector, indexSelector])

data Expr
  = And Expr Expr
  | Or Expr Expr
  | Getter Getter
  deriving (Show)

orExprParser :: Parser Expr
orExprParser = do
  _ <- B.string "or"
  _ <- byteChar ' '
  expr1 <- exprParser
  _ <- byteChar ' '
  Or expr1 <$> exprParser

andExprParser :: Parser Expr
andExprParser = do
  _ <- B.string "and"
  _ <- byteChar ' '
  expr1 <- exprParser
  _ <- byteChar ' '
  And expr1 <$> exprParser

getterExprParser :: Parser Expr
getterExprParser = Getter <$> getterParser

exprParser :: Parser Expr
exprParser = tryParsers [orExprParser, andExprParser, getterExprParser]

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

runGetter :: Getter -> MapForest -> Either Selector MapForest
runGetter funs = foldl' (>=>) id (map select funs) . Right
