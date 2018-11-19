{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module Data.Aeson.JSONString (JSONString (..)) where

import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import Data.String (IsString)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances ()


newtype JSONString = JSONString
  { getJSONString :: Text
  } deriving (Eq, Ord, Generic, ToJSON, FromJSON, NFData, IsString, Arbitrary)
