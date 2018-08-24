{-# LANGUAGE
    OverloadedStrings
  #-}

import Test.Serialization (ServerParams (..), startServer)
import Test.Serialization.Types
  (TestSuiteM, registerTopic, ChannelMsg, ClientToServer, ServerToClient)
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
  --   [ QC.testProperty "ChannelMsg" $ \x ->
  --       let go = unsafePerformIO $ do
  --             print x
  --             pure id
  --       in  go (jsonIso (x :: ChannelMsg))
  --   ]
  startServer
    ServerParams
    { serverParamsControlHost = URIAuth Strict.Nothing Glob (Strict.Just 5561)
    , serverParamsTestSuite = tests
    }


tests :: TestSuiteM ()
tests = do
  registerTopic "ChannelMsg" (Proxy :: Proxy ChannelMsg)
  registerTopic "ClientToServer" (Proxy :: Proxy ClientToServer)
  registerTopic "ServerToClient" (Proxy :: Proxy ServerToClient)


jsonIso :: ToJSON a => FromJSON a => Eq a => a -> Result
jsonIso x = case decode (encode x) of
  Nothing -> failed
  Just y
    | y == x -> succeeded
    | otherwise -> failed
