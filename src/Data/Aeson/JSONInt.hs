{-# LANGUAGE
    DeriveGeneric
  , GeneralizedNewtypeDeriving
  #-}

module Data.Aeson.JSONInt where

import Data.Aeson (ToJSON, FromJSON)
import Data.Int (Int32)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (scale)
import System.IO.Unsafe (unsafePerformIO)


newtype JSONInt = JSONInt
  { getJSONInt :: Int32
  } deriving (Eq, Ord, Enum, Show, Read, Generic, Num, Real, Integral, ToJSON, FromJSON, Arbitrary)
