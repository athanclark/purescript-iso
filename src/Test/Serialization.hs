{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , RecordWildCards
  #-}

module Test.Serialization where

import Test.Serialization.Types
  ( TestSuiteM, ChannelMsg (..), ServerToClientControl (..)
  , ClientToServerControl (..), TestSuiteState, emptyTestSuiteState)

import Data.URI (URI (..), printURI)
import Data.URI.Auth (URIAuth)
import Data.UUID (UUID)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Strict.Maybe as Strict
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson (decode, encode)
import Control.Monad (forever, void)
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control (liftBaseWith)
import Control.Concurrent.Async (async, link, cancel)
import Control.Concurrent.STM
  (STM, TVar, newTVar, atomically, modifyTVar, readTVar)
import System.ZMQ4.Monadic
  (runZMQ, socket, bind, Rep (..), Push (..), Pull (..), receive, send')


data ServerParams = ServerParams
  { serverParamsControlHost :: URIAuth
  , serverParamsChannelOutHost :: URIAuth
  , serverParamsChannelInHost :: URIAuth
  , serverParamsTestSuite :: TestSuiteM ()
  }


type ServerState = TVar (Map UUID TestSuiteState)

emptyServerState :: STM ServerState
emptyServerState = newTVar Map.empty



startServer :: ServerParams -> IO ()
startServer ServerParams{..} = do
  runZMQ $ do
    controlService <- socket Rep
    bind controlService $ T.unpack $ printURI $
      URI (Strict.Just "tcp") True serverParamsControlHost [] [] Strict.Nothing

    channelServiceIn <- socket Pull
    bind channelServiceIn $ T.unpack $ printURI $
      URI (Strict.Just "tcp") True serverParamsChannelInHost [] [] Strict.Nothing

    channelServiceOut <- socket Push
    bind channelServiceOut $ T.unpack $ printURI $
      URI (Strict.Just "tcp") True serverParamsChannelOutHost [] [] Strict.Nothing

    serverStateRef <- liftIO $ atomically emptyServerState

    firstCtlMsgBS <- receive controlService

    case decode (LBS.fromStrict firstCtlMsgBS) of
      Nothing -> do
        let err = BadParse (T.decodeUtf8 firstCtlMsgBS)
        send' controlService [] (encode err)
        error $ show err
      Just x -> case x of
        ClientRegister clientKey ->
          liftIO $ registerClient serverParamsTestSuite serverStateRef clientKey
        ClientDeRegister -> error "client DeRegistered without registering..."

    deregisterThread <- liftBaseWith $ \runInBase ->
      async $ forever $ void $ runInBase $ do
        asyncCtlMsgBS <- receive controlService
        case decode (LBS.fromStrict asyncCtlMsgBS) of
          Nothing -> do
            let err = BadParse (T.decodeUtf8 asyncCtlMsgBS)
            send' controlService [] (encode err)
            error $ show err
          Just x -> case x of
            ClientDeRegister -> undefined -- unregister
            ClientRegister k -> error $ "client Registered out of nowhere: " ++ show k

    liftIO (link deregisterThread)

    forever $ do
      asyncChnMsgBS <- receive channelServiceIn
      case decode (LBS.fromStrict asyncChnMsgBS) of
        Nothing -> do
          let err = BadParse (T.decodeUtf8 asyncChnMsgBS)
          send' controlService [] (encode err)
          liftIO (cancel deregisterThread)
          error $ show err
        Just x -> case x of
          GeneratedInput _ _ -> undefined -- verify state & continue
          Serialized _ _ -> undefined -- verify state & continue
          DeSerialized _ _ -> undefined -- verify state & continue or halt
          Failure _ _ -> undefined -- halt and report


-- processChannelMsg :: TestSuiteState
--                   -> ChannelMsg
--                   -> 


registerClient :: TestSuiteM ()
               -> ServerState
               -> UUID
               -> IO ()
registerClient tests serverState clientKey = do
  ss <- atomically $ readTVar serverState
  case Map.lookup clientKey ss of
    Just _ -> error "Client key already taken"
    Nothing -> do
      suiteState <- atomically $ do
        x <- emptyTestSuiteState
        modifyTVar serverState $ Map.insert clientKey x
        pure x
      runReaderT tests suiteState
