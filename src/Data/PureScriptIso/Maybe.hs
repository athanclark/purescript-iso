{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , DeriveGeneric
  , DeriveAnyClass
  #-}

module Data.PureScriptIso.Maybe where

import Prelude hiding (Maybe (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (String, Array))
import Data.Aeson.Types (typeMismatch)
import qualified Data.Vector as V
import Control.DeepSeq (NFData)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import GHC.Generics (Generic)


data Maybe a
  = Nothing
  | Just a
  deriving (Eq, Ord, Show, Generic, NFData)

instance (ToJSON a) => ToJSON (Maybe a) where
  toJSON x = case x of
    Nothing -> String ""
    Just y -> Array [toJSON y]

instance (FromJSON a) => FromJSON (Maybe a) where
  parseJSON json = case json of
    String s
      | s == "" -> pure Nothing
      | otherwise -> fail'
    Array a
      | length a == 1 -> Just <$> parseJSON (a V.! 0)
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "Maybe" json

instance (Arbitrary a) => Arbitrary (Maybe a) where
  arbitrary = oneof
    [ pure Nothing
    , Just <$> arbitrary
    ]
