module Lib
  ( MapForest(..)
  , manipulateContents
  ) where

import Relude hiding (ByteString)
import Relude.Bool (Bool(..))

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types ()

import Data.Map (Map)
import qualified Data.Map as Map

import Data.ByteString.Lazy.UTF8 (ByteString)

data MapForest
  = Tree (Map Text MapForest)
  | Branch (V.Vector MapForest)
  | Leaf Text
  deriving (Show, Eq)

instance Aeson.FromJSON MapForest where
  parseJSON (Aeson.Object o) = do
    forestPairs <-
      traverse (\(k, v) -> (,) (K.toText k) <$> Aeson.parseJSON v) (KM.toList o)
    pure . Tree . Map.fromList $ forestPairs
  parseJSON Aeson.Null = pure (Leaf "")
  parseJSON (Aeson.String s) = pure . Leaf . wrap $ s
    where
      wrap t = T.cons '"' $ T.snoc t '"'
  parseJSON (Aeson.Number n) = pure . Leaf . show $ n
  parseJSON (Aeson.Bool b) = pure . Leaf . show $ b
  parseJSON (Aeson.Array a) = Branch <$> traverse Aeson.parseJSON a

instance Aeson.ToJSON MapForest where
  toJSON (Tree m) =
    Aeson.object [K.fromText k .= Aeson.toJSON v | (k, v) <- Map.toList m]
  toJSON (Branch forests) = Aeson.Array . V.map Aeson.toJSON $ forests
  toJSON (Leaf s) = Aeson.toJSON . parseValue $ s

parseValue :: Text -> Maybe Aeson.Value
parseValue "true" = Just (Aeson.Bool True)
parseValue "false" = Just (Aeson.Bool False)
parseValue str =
  if T.all (`T.elem` toText ['0' .. '9']) str
    then fmap Aeson.Number (readMaybe . toString $ str)
    else let stripped = T.tail . T.init $ str
          in Just (Aeson.String stripped)

notEmptyForest :: MapForest -> Bool
notEmptyForest (Tree m) = not $ Map.null m
notEmptyForest (Branch vec) = not $ V.null vec
notEmptyForest _ = True

hasFilteredChild :: (MapForest -> Bool) -> MapForest -> Bool
hasFilteredChild p (Tree m) = any p (Map.elems m)
hasFilteredChild p (Branch vec) = any (\v -> p v || hasFilteredChild p v) vec
hasFilteredChild _ _ = False

isLeaf :: MapForest -> Bool
isLeaf (Leaf _) = True
isLeaf _ = False

filterMapForest :: (MapForest -> Bool) -> Bool -> MapForest -> MapForest
filterMapForest p True tree@(Tree m) =
  Tree $ Map.union (Map.filter isLeaf m) $ fromMaybe m filteredForest
  where
    getMap (Tree m') = Just m'
    getMap _ = Nothing
    filteredForest = getMap (filterMapForest p False tree)
filterMapForest p False (Tree m) =
  let filteredMap =
        Map.filterWithKey (\k v -> p v || p (Leaf k) || hasFilteredChild p v) m
   in Tree .
      Map.filter notEmptyForest .
      Map.mapWithKey (\k v -> filterMapForest p (p (Leaf k)) v) $
      filteredMap
filterMapForest p True (Branch forests) =
  Branch . V.map (filterMapForest p True) $ forests
filterMapForest p False (Branch forests) =
  let filteredList = V.filter (\v -> p v || hasFilteredChild p v) forests
   in Branch . V.filter notEmptyForest . V.map (filterMapForest p False) $
      filteredList
filterMapForest _ _ leaf@(Leaf _) = leaf

manipulateContents :: String -> MapForest -> ByteString
manipulateContents searchFor mapForest =
  Aeson.encode
    (filterMapForest (predicate searchFor) False mapForest)

containsSubstring :: Text -> MapForest -> Bool
containsSubstring a (Tree m) =
  any (T.isInfixOf (T.toLower a)) (Map.keys m) ||
  any (containsSubstring a) (Map.elems m)
containsSubstring a (Branch vec) = any (containsSubstring a) vec
containsSubstring a (Leaf s) = T.isInfixOf a (T.toLower s)

predicate :: String -> MapForest -> Bool
predicate a = containsSubstring (T.toLower . toText $ a)
