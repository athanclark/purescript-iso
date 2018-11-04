{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , DeriveAnyClass
  #-}

module Data.Aeson.JSONUnit where

import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Control.DeepSeq (NFData)
import Test.QuickCheck (Arbitrary (..))
import GHC.Generics (Generic)


data JSONUnit = JSONUnit
  deriving (Eq, Ord, Show, Generic, NFData)

instance Arbitrary JSONUnit where
  arbitrary = pure JSONUnit

instance ToJSON JSONUnit where
  toJSON JSONUnit = String ""

instance FromJSON JSONUnit where
  parseJSON json = case json of
    String x
      | x == "" -> pure JSONUnit
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "JSONUnit" json

