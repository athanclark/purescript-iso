{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module Data.Aeson.JSONEmailAddress where

import Text.EmailAddress (EmailAddress, emailAddressFromString)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Char as Char
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (listOf1, elements, scale)
import GHC.Generics (Generic)


newtype JSONEmailAddress = JSONEmailAddress
  { getJSONEmailAddress :: EmailAddress
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Arbitrary JSONEmailAddress where
  arbitrary = do
    name <- arbitraryNonEmptyAscii -- listOf1 (arbitrary `suchThat` Char.isAlphaNum)
    domain <- arbitraryNonEmptyAscii -- listOf1 (arbitrary `suchThat` Char.isAlphaNum)
    let x = name ++ "@" ++ domain ++ ".com"
    case emailAddressFromString x of
      Just e -> pure (JSONEmailAddress e)
      Nothing -> error x
    where
      arbitraryNonEmptyAscii = scale (`div` 2) $ listOf1 (elements ['a' .. 'z'])
