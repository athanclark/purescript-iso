{-# LANGUAGE
    OverloadedStrings
  #-}

import Test.Serialization (ServerParams (..), startServer)
import Test.Serialization.Types
  (TestSuiteM, registerTopic, ChannelMsg, ClientToServer, ServerToClient)

import Data.Proxy (Proxy (..))
import Data.Strict.Maybe as Strict
import Data.URI.Auth (URIAuth (..))
import Data.URI.Auth.Host (URIAuthHost (Glob))


main :: IO ()
main = do
  putStrLn "Starting tests..."
  startServer
    ServerParams
    { serverParamsControlHost = URIAuth Strict.Nothing Glob (Strict.Just 5561)
    , serverParamsTestSuite = tests
    }


tests :: TestSuiteM ()
tests = do
  pure ()
  -- registerTopic "ChannelMsg" (Proxy :: Proxy ChannelMsg)
  -- registerTopic "ClientToServer" (Proxy :: Proxy ClientToServer)
  -- registerTopic "ServerToClient" (Proxy :: Proxy ServerToClient)
