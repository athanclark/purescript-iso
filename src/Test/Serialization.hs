{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , RecordWildCards
  #-}

module Test.Serialization where

import Test.Serialization.Types
  ( TestSuiteM, ChannelMsg (..), ServerToClientControl (..)
  , ClientToServerControl (..))

import Data.URI (URI (..), printURI)
import Data.URI.Auth (URIAuth)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Strict.Maybe as Strict
import qualified Data.Map.Strict as Map
import Data.Aeson (decode, encode)
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM (newTVarIO)
import System.ZMQ4.Monadic
  (runZMQ, socket, bind, Rep (..), Pub (..), receive, send')


data ServerParams = ServerParams
  { serverParamsControlHost :: URIAuth
  , serverParamsChannelHost :: URIAuth
  , serverParamsTestSuite :: TestSuiteM ()
  }


startServer :: ServerParams -> IO ()
startServer ServerParams{..} = do
  runZMQ $ do
    controlService <- socket Rep
    bind controlService $ T.unpack $ printURI $
      URI (Strict.Just "tcp") True serverParamsControlHost [] [] Strict.Nothing

    channelService <- socket Pub
    bind channelService $ T.unpack $ printURI $
      URI (Strict.Just "tcp") True serverParamsChannelHost [] [] Strict.Nothing

    liftIO $ do
      compiledRef <- newTVarIO Map.empty
      runReaderT serverParamsTestSuite compiledRef

    firstCtlMsgBS <- receive controlService

    case decode (LBS.fromStrict firstCtlMsgBS) of
      Nothing -> send' controlService [] $ encode $ BadParse $ T.decodeUtf8 firstCtlMsgBS
      Just x -> case x of
        ClientRegister key -> undefined
        ClientDeRegister -> undefined


-- processChannelMsg :: TestSuiteState
--                   -> ChannelMsg
--                   -> 
