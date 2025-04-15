module Lib
  ( manipulateContents
  ) where

import Relude hiding (ByteString)
import Relude.Bool (Bool(..))

import Data.Aeson qualified as Aeson (encode)

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Vector qualified as V

import Data.ByteString.Lazy.UTF8 (ByteString)
import MapForest
import Query

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
  Tree . Map.union (Map.filter isLeaf m) $ fromMaybe m filteredForest
  where
    getMap (Tree m') = Just m'
    getMap _ = Nothing
    filteredForest = getMap (filterMapForest p False tree)
filterMapForest p False (Tree m) =
  let filteredMap =
        Map.filterWithKey (\k v -> p v || p (Leaf k) || hasFilteredChild p v) m
   in Tree
        . Map.filter notEmptyForest
        . Map.mapWithKey (filterMapForest p . p . Leaf)
        $ filteredMap
filterMapForest p True (Branch forests) =
  Branch . V.map (filterMapForest p True) $ forests
filterMapForest p False (Branch forests) =
  let filteredList = V.filter (\v -> p v || hasFilteredChild p v) forests
   in Branch . V.filter notEmptyForest . V.map (filterMapForest p False)
        $ filteredList
filterMapForest _ _ leaf@(Leaf _) = leaf

manipulateContents :: Query -> MapForest -> ByteString
manipulateContents (SimpleQuery searchFor) mapForest =
  Aeson.encode (filterMapForest (predicate searchFor) False mapForest)
manipulateContents (AdvancedQuery searchFor) mapForest =
  Aeson.encode (advancedQuery (encodeUtf8 searchFor) mapForest)

containsSubstring :: Text -> MapForest -> Bool
containsSubstring a (Tree m) =
  any (T.isInfixOf (T.toLower a)) (Map.keys m)
    || any (containsSubstring a) (Map.elems m)
containsSubstring a (Branch vec) = any (containsSubstring a) vec
containsSubstring a (Leaf s) = T.isInfixOf a (T.toLower s)

predicate :: Text -> MapForest -> Bool
predicate a = containsSubstring (T.toLower . toText $ a)
