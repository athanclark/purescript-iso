{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module Data.Aeson.JSONEmailAddress where

import Text.EmailAddress (EmailAddress, emailAddressFromString)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Char as Char
import Control.Monad (replicateM)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (elements, scale, choose)
import GHC.Generics (Generic)

-- FIXME restrict to 64 x 63 chars

newtype JSONEmailAddress = JSONEmailAddress
  { getJSONEmailAddress :: EmailAddress
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance Arbitrary JSONEmailAddress where
  arbitrary = do
    name <- arbitraryNonEmptyAscii 64
    domain <- arbitraryNonEmptyAscii 63
    let x = name ++ "@" ++ domain ++ ".com"
    case emailAddressFromString x of
      Just e -> pure (JSONEmailAddress e)
      Nothing -> error x
    where
      arbitraryNonEmptyAscii s = do
        l <- choose (1,s)
        replicateM l (elements ['a' .. 'z'])
