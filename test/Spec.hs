{-# LANGUAGE
    OverloadedStrings
  #-}

import Data.Aeson.JSONUnit (JSONUnit)
import Data.Aeson.JSONEither (JSONEither)
import Data.Aeson.JSONMaybe (JSONMaybe)
import Data.Aeson.JSONTuple (JSONTuple)
import Data.Aeson.JSONDateTime (JSONDateTime)
import Data.Aeson.JSONString (JSONString)
import Data.Aeson.JSONEmailAddress (JSONEmailAddress)
import Data.Aeson.JSONInt (JSONInt)
import Data.Aeson.JSONInteger (JSONInteger)
import Data.Aeson.JSONScientific (JSONScientific)
import Data.Time (UTCTime)
import Data.Time.Calendar (Day)

import Test.Serialization (ServerParams (..), startServer)
import Test.Serialization.Types
  (TestSuiteM, registerTopic, MsgType, ClientToServer, ServerToClient, TestTopic)
import Test.Tasty (defaultMain, testGroup)
import qualified Test.Tasty.QuickCheck as QC
import Test.QuickCheck.Property (succeeded, failed, Result)

import Data.Aeson (ToJSON, FromJSON, encode, decode)
import Data.Proxy (Proxy (..))
import qualified Data.Strict.Maybe as Strict
import Data.URI.Auth (URIAuth (..))
import Data.URI.Auth.Host (URIAuthHost (Glob))
import System.IO.Unsafe (unsafePerformIO)


main :: IO ()
main = do
  putStrLn "Starting tests..."
  -- defaultMain $ testGroup "Self-check"
  --   [ QC.testProperty "MsgType" $ \x ->
  --       let go = unsafePerformIO $ do
  --             print x
  --             pure id
  --       in  go (jsonIso (x :: MsgType))
  --   ]
  startServer
    ServerParams
    { serverParamsControlHost = URIAuth Strict.Nothing Glob (Strict.Just 5561)
    , serverParamsTestSuite = tests
    , serverParamsMaxSize = 200
    }


tests :: TestSuiteM ()
tests = do
  registerTopic "TestTopic" (Proxy :: Proxy TestTopic)
  registerTopic "MsgType" (Proxy :: Proxy MsgType)
  registerTopic "ClientToServer" (Proxy :: Proxy ClientToServer)
  registerTopic "ServerToClient" (Proxy :: Proxy ServerToClient)
  registerTopic "JSONUnit" (Proxy :: Proxy JSONUnit)
  registerTopic "JSONEither" (Proxy :: Proxy (JSONEither JSONUnit JSONUnit))
  registerTopic "JSONMaybe" (Proxy :: Proxy (JSONMaybe JSONUnit))
  registerTopic "JSONTuple" (Proxy :: Proxy (JSONTuple JSONUnit JSONUnit))
  registerTopic "JSONDate" (Proxy :: Proxy Day)
  registerTopic "JSONDateTime" (Proxy :: Proxy JSONDateTime)
  registerTopic "JSONString" (Proxy :: Proxy JSONString)
  registerTopic "JSONEmailAddress" (Proxy :: Proxy JSONEmailAddress)
  registerTopic "JSONInt" (Proxy :: Proxy JSONInt)
  registerTopic "JSONInteger" (Proxy :: Proxy JSONInteger)
  registerTopic "JSONScientific" (Proxy :: Proxy JSONScientific)


jsonIso :: ToJSON a => FromJSON a => Eq a => a -> Result
jsonIso x = case decode (encode x) of
  Nothing -> failed
  Just y
    | y == x -> succeeded
    | otherwise -> failed
