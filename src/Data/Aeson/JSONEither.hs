{-# LANGUAGE
    OverloadedStrings
  #-}

module Data.Aeson.JSONEither where

import Data.Aeson (ToJSON (..), FromJSON (..), Value (Object), object, (.=), (.:))
import Data.Aeson.Types (typeMismatch)
import Control.Applicative ((<|>))
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (oneof)


data JSONEither a b
  = JSONLeft a
  | JSONRight b
  deriving (Eq, Show)

instance (ToJSON a, ToJSON b) => ToJSON (JSONEither a b) where
  toJSON x = case x of
    JSONLeft a -> object ["e" .= a]
    JSONRight b -> object ["x" .= b]

instance (FromJSON a, FromJSON b) => FromJSON (JSONEither a b) where
  parseJSON json = case json of
    Object o -> do
      let e = JSONLeft <$> o .: "e"
          x = JSONRight <$> o .: "x"
      e <|> x
    _ -> fail'
    where
      fail' = typeMismatch "JSONEither" json

instance (Arbitrary a, Arbitrary b) => Arbitrary (JSONEither a b) where
  arbitrary = oneof
    [ JSONLeft <$> arbitrary
    , JSONRight <$> arbitrary
    ]
