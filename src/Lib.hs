module Lib
  ( MapForest(..)
  , manipulateContents
  ) where

import qualified Data.Text as T (init, pack, tail, unpack)
import qualified Data.Vector as V

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types ()

import Data.Map (Map)
import qualified Data.Map as Map

import Data.Char (toLower)
import Data.List (isInfixOf)

import Text.Read (readMaybe)

import Data.ByteString.Lazy.UTF8 (ByteString)

data MapForest
  = Tree (Map String MapForest)
  | Branch [MapForest]
  | Leaf String
  deriving (Show, Eq)

instance Aeson.FromJSON MapForest where
  parseJSON (Aeson.Object o) = do
    forestPairs <-
      traverse
        (\(k, v) -> (,) <$> pure (T.unpack . K.toText $ k) <*> Aeson.parseJSON v)
        (KM.toList o)
    pure $ Tree (Map.fromList forestPairs)
  parseJSON (Aeson.Null) = pure (Leaf "")
  parseJSON (Aeson.String s) = pure . Leaf . show . T.unpack $ s
  parseJSON (Aeson.Number n) = pure . Leaf . show $ n
  parseJSON (Aeson.Bool b) = pure . Leaf . show $ b
  parseJSON (Aeson.Array a) = Branch <$> traverse Aeson.parseJSON (V.toList a)

instance Aeson.ToJSON MapForest where
  toJSON (Tree m) =
    Aeson.object
      [(K.fromText . T.pack $ k) .= Aeson.toJSON v | (k, v) <- Map.toList m]
  toJSON (Branch forests) = Aeson.Array $ V.fromList $ fmap Aeson.toJSON forests
  toJSON (Leaf s) = Aeson.toJSON . parseValue $ s

parseValue :: String -> Maybe Aeson.Value
parseValue "true" = Just (Aeson.Bool True)
parseValue "false" = Just (Aeson.Bool False)
parseValue str =
  if all (`elem` ['0' .. '9']) str
    then case readMaybe str of
           Just sci -> Just (Aeson.Number sci)
           Nothing -> Nothing
    else let stripped = T.tail . T.init . T.pack $ str
          in Just (Aeson.String stripped)

notEmptyForest :: MapForest -> Bool
notEmptyForest (Tree m) = not $ Map.null m
notEmptyForest (Branch []) = False
notEmptyForest (Leaf "") = False
notEmptyForest _ = True

hasFilteredChild :: (MapForest -> Bool) -> MapForest -> Bool
hasFilteredChild p (Tree m) = any p (Map.elems m)
hasFilteredChild p (Branch l) = any (\v -> p v || hasFilteredChild p v) l
hasFilteredChild p leaf@(Leaf _) = p leaf

filterMapForest :: (MapForest -> Bool) -> MapForest -> MapForest
filterMapForest p (Tree m) =
  let filteredMap =
        Map.filterWithKey (\k v -> p v || p (Leaf k) || hasFilteredChild p v) m
      nonEmptyMap =
        Tree . Map.filter notEmptyForest . Map.map (filterMapForest p) $
        filteredMap
   in nonEmptyMap
filterMapForest p (Branch forests) =
  let filteredList = filter (\v -> p v || hasFilteredChild p v) forests
   in Branch . filter notEmptyForest . map (filterMapForest p) $ filteredList
filterMapForest _ leaf@(Leaf _) = leaf

manipulateContents :: [Char] -> MapForest -> ByteString
manipulateContents searchFor mapForest =
  Aeson.encode (filterMapForest (predicate (map toLower searchFor)) mapForest)

containsSubstring :: String -> MapForest -> Bool
containsSubstring a (Tree m) =
  any (isInfixOf (map toLower a)) (Map.keys m) ||
  any (containsSubstring a) (Map.elems m)
containsSubstring a (Branch l) = any (containsSubstring a) l
containsSubstring a (Leaf s) = isInfixOf a (map toLower s)

predicate :: String -> MapForest -> Bool
predicate a node = containsSubstring a node
