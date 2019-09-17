{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , DeriveAnyClass
  #-}

module Data.PureScriptIso.Unit where

import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Control.DeepSeq (NFData)
import Test.QuickCheck (Arbitrary (..))
-- import Test.Serialization.Symbiote (SymbioteOperation (..))
import GHC.Generics (Generic)


data Unit = Unit
  deriving (Eq, Ord, Show, Generic, NFData)

instance Arbitrary Unit where
  arbitrary = pure Unit

instance ToJSON Unit where
  toJSON Unit = String ""

instance FromJSON Unit where
  parseJSON json = case json of
    String x
      | x == "" -> pure Unit
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "Unit" json

-- instance SymbioteOperation Unit where
  
