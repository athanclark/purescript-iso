{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module Data.PureScriptIso.String (String (..)) where

import Prelude hiding (String)
import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import Data.String (IsString)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary)
import Test.QuickCheck.Instances ()


newtype String = String
  { getString :: Text
  } deriving (Eq, Ord, Generic, ToJSON, FromJSON, NFData, IsString, Arbitrary)
