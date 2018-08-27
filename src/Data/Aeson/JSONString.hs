{-# LANGUAGE
    OverloadedStrings
  , GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module Data.Aeson.JSONString (JSONString, jsonString, getJSONString) where

import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (ToJSON, FromJSON)
import GHC.Generics (Generic)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Instances ()


newtype JSONString = JSONString Text
  deriving (Eq, Ord, Generic, ToJSON, FromJSON)


jsonString :: Text -> JSONString
jsonString = JSONString -- . T.pack . show


getJSONString :: JSONString -> Text
getJSONString (JSONString x) = x -- T.pack $ read $ T.unpack x


instance Arbitrary JSONString where
  arbitrary = jsonString <$> arbitrary
