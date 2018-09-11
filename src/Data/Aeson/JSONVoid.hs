{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , DeriveAnyClass
  #-}

module Data.Aeson.JSONVoid where

import Data.Aeson (ToJSON (..), FromJSON (..), Value (String))
import Data.Aeson.Types (typeMismatch)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)


data JSONVoid
  deriving (Generic, NFData)

instance ToJSON JSONVoid where
  toJSON _ = String ""

instance FromJSON JSONVoid where
  parseJSON = typeMismatch "JSONVoid"
