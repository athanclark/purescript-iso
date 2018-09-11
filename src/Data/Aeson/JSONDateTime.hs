{-# LANGUAGE
    GeneralizedNewtypeDeriving
  , DeriveGeneric
  #-}

module Data.Aeson.JSONDateTime where

import Data.Time
  ( UTCTime, formatTime, iso8601DateFormat, defaultTimeLocale
  , getCurrentTime)
import Data.Aeson (ToJSON, FromJSON, decode)
import qualified Data.ByteString.Lazy.UTF8 as LBS8
import Control.DeepSeq (NFData)
import Test.QuickCheck (Arbitrary (..))
import GHC.Generics (Generic)
import System.IO.Unsafe (unsafePerformIO)


newtype JSONDateTime = JSONDateTime
  { getJSONDateTime :: UTCTime
  } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, NFData)


jsonDateTime :: UTCTime -> JSONDateTime
jsonDateTime now =
  let p = take 3 (formatTime defaultTimeLocale "%q" now)
      s = formatTime defaultTimeLocale (iso8601DateFormat $ Just "%H:%M:%S") now
      s' = s ++ "." ++ p ++ "Z"
  in  case decode $ LBS8.fromString $ show s' of
        Just x -> JSONDateTime x
        Nothing -> error s'


instance Arbitrary JSONDateTime where
  arbitrary =
    let go = unsafePerformIO (jsonDateTime <$> getCurrentTime)
    in  go `seq` pure go
