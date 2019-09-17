{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module Data.PureScriptIso.EmailAddress where

import qualified Text.EmailAddress as E
import Data.Aeson (ToJSON, FromJSON)
import Control.Monad (replicateM)
import Control.DeepSeq (NFData (..))
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (elements, choose)
import GHC.Generics (Generic)

-- FIXME restrict to 64 x 63 chars

newtype EmailAddress = EmailAddress
  { getEmailAddress :: E.EmailAddress
  } deriving (Eq, Ord, Show, Generic, FromJSON, ToJSON)

instance NFData EmailAddress where
  rnf (EmailAddress x) = seq x ()

instance Arbitrary EmailAddress where
  arbitrary = do
    name <- arbitraryNonEmptyAscii 64
    domain <- arbitraryNonEmptyAscii 63
    let x = name ++ "@" ++ domain ++ ".com"
    case E.emailAddressFromString x of
      Just e -> pure (EmailAddress e)
      Nothing -> error x
    where
      arbitraryNonEmptyAscii s = do
        l <- choose (1,s)
        replicateM l (elements ['a' .. 'z'])
