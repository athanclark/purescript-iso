{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , DeriveAnyClass
  #-}

module Data.PureScriptIso.Tuple where

import Data.Aeson (ToJSON (..), FromJSON (..), Value (Object), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import Control.DeepSeq (NFData)
import Test.QuickCheck (Arbitrary (..))
import GHC.Generics (Generic)


data Tuple a b = Tuple a b
  deriving (Eq, Ord, Show, Generic, NFData)

instance (ToJSON a, ToJSON b) => ToJSON (Tuple a b) where
  toJSON (Tuple a b) = object ["l" .= a, "r" .= b]

instance (FromJSON a, FromJSON b) => FromJSON (Tuple a b) where
  parseJSON json = case json of
    Object o -> Tuple <$> o .: "l" <*> o .: "r"
    _ -> fail'
    where
      fail' = typeMismatch "Tuple" json

instance (Arbitrary a, Arbitrary b) => Arbitrary (Tuple a b) where
  arbitrary = Tuple <$> arbitrary <*> arbitrary


uncurryTuple :: (a -> b -> c) -> Tuple a b -> c
uncurryTuple f (Tuple a b) = f a b
