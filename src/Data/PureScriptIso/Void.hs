{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , DeriveAnyClass
  #-}

module Data.PureScriptIso.Void where

import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)


data Void
  deriving (Generic, NFData)

instance ToJSON Void where
  toJSON _ = String ""

instance FromJSON Void where
  parseJSON = typeMismatch "Void"
