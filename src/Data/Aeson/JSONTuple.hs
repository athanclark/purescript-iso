{-# LANGUAGE
    OverloadedStrings
  #-}

module Data.Aeson.JSONTuple where

import Data.Aeson (ToJSON (..), FromJSON (..), Value (Object), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import Test.QuickCheck (Arbitrary (..))


data JSONTuple a b = JSONTuple a b
  deriving (Eq, Show)

instance (ToJSON a, ToJSON b) => ToJSON (JSONTuple a b) where
  toJSON (JSONTuple a b) = object ["l" .= a, "r" .= b]

instance (FromJSON a, FromJSON b) => FromJSON (JSONTuple a b) where
  parseJSON json = case json of
    Object o -> JSONTuple <$> o .: "l" <*> o .: "r"
    _ -> fail'
    where
      fail' = typeMismatch "JSONTuple" json

instance (Arbitrary a, Arbitrary b) => Arbitrary (JSONTuple a b) where
  arbitrary = JSONTuple <$> arbitrary <*> arbitrary


uncurryJSONTuple :: (a -> b -> c) -> JSONTuple a b -> c
uncurryJSONTuple f (JSONTuple a b) = f a b
