{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.PureScriptIso.Int where

import Data.Aeson (ToJSON, FromJSON)
import Data.Int (Int32)
import Control.DeepSeq (NFData)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))


newtype Int = Int
  { getInt :: Int32
  } deriving ( Eq, Ord, Enum, Show, Read, Generic, Num, Real, Integral
             , ToJSON, FromJSON, Arbitrary, NFData)
