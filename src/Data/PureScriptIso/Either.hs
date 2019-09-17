{-# LANGUAGE
    OverloadedStrings
  , DeriveGeneric
  , DeriveAnyClass
  #-}

module Data.PureScriptIso.Either where

import Prelude hiding (Either (..))
import Data.Aeson (ToJSON (..), FromJSON (..), Value (Object), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import Control.DeepSeq (NFData)
import Control.Applicative ((<|>))
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)
import GHC.Generics (Generic)


data Either a b
  = Left a
  | Right b
  deriving (Eq, Ord, Show, Generic, NFData)

instance (ToJSON a, ToJSON b) => ToJSON (Either a b) where
  toJSON x = case x of
    Left a -> object ["e" .= a]
    Right b -> object ["x" .= b]

instance (FromJSON a, FromJSON b) => FromJSON (Either a b) where
  parseJSON json = case json of
    Object o -> do
      let e = Left <$> o .: "e"
          x = Right <$> o .: "x"
      e <|> x
    _ -> fail'
    where
      fail' = typeMismatch "Either" json

instance (Arbitrary a, Arbitrary b) => Arbitrary (Either a b) where
  arbitrary = oneof
    [ Left <$> arbitrary
    , Right <$> arbitrary
    ]
