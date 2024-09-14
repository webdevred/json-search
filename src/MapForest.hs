module MapForest (MapForest(..)) where

import Relude hiding (ByteString)
import Relude.Bool (Bool(..))

import qualified Data.Vector as V
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Text (Text)
import qualified Data.Text as T

import Data.Aeson ((.=))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Types ()

unwrap :: Text -> Maybe Text
unwrap = T.stripSuffix "\"" >=> T.stripPrefix "\""

parseValue :: Text -> Maybe Aeson.Value
parseValue "true" = Just (Aeson.Bool True)
parseValue "false" = Just (Aeson.Bool False)
parseValue str =
  fmap Aeson.String (unwrap str) <|>
  fmap Aeson.Number (readMaybe . toString $ str)

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
