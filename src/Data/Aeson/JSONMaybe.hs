{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , DeriveGeneric
  , DeriveAnyClass
  #-}

module Data.Aeson.JSONMaybe where

import Data.Aeson (ToJSON (..), FromJSON (..), Value (String, Array))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as V
import Control.DeepSeq (NFData)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import GHC.Generics (Generic)


data JSONMaybe a
  = JSONNothing
  | JSONJust a
  deriving (Eq, Ord, Show, Generic, NFData)

instance (ToJSON a) => ToJSON (JSONMaybe a) where
  toJSON x = case x of
    JSONNothing -> String ""
    JSONJust y -> Array [toJSON y]

instance (FromJSON a) => FromJSON (JSONMaybe a) where
  parseJSON json = case json of
    String s
      | s == "" -> pure JSONNothing
      | otherwise -> fail'
    Array a
      | length a == 1 -> JSONJust <$> parseJSON (a V.! 0)
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "JSONMaybe" json

instance (Arbitrary a) => Arbitrary (JSONMaybe a) where
  arbitrary = oneof
    [ pure JSONNothing
    , JSONJust <$> arbitrary
    ]
