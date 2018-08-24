{-# LANGUAGE
    OverloadedStrings
  #-}

module Data.Aeson.JSONUnit where

import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Test.QuickCheck (Arbitrary (..))


data JSONUnit = JSONUnit
  deriving (Eq, Show)

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


boolToUnit :: Bool -> Maybe JSONUnit
boolToUnit ok
  | ok = Just JSONUnit
  | otherwise = Nothing


unitToUnit :: () -> Maybe JSONUnit
unitToUnit () = Just JSONUnit
