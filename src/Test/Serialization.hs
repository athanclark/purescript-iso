{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , RecordWildCards
  , ScopedTypeVariables
  #-}

module Test.Serialization where

import Test.Serialization.Types
  ( TestSuiteM, ChannelMsg (..), ServerToClient (..), TestTopic
  , ClientToServer (..), TestSuiteState, emptyTestSuiteState
  , gotClientGenValue, serializeValueClientOrigin
  , gotClientSerialize, gotClientDeSerialize
  , deserializeValueClientOrigin, verify, generateValue
  , HasTopic (..), DesValue (..), HasClientG (..), GenValue (..)
  , HasClientS (..), isOkay)

import Data.URI (URI (..), printURI)
import Data.URI.Auth (URIAuth)
import Data.UUID (UUID)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Strict.Maybe as Strict
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson (decode, encode)
import Control.Monad (forever, void, when)
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control (liftBaseWith)
import Control.Concurrent.Async (async, link, cancel)
import Control.Concurrent.STM
  (STM, TVar, newTVar, atomically, modifyTVar, readTVar)
import System.ZMQ4.Monadic
  (runZMQ, Router (..), Dealer (..))
import qualified System.ZMQ4.Simple as Z
import System.Exit (exitSuccess)


data ServerParams = ServerParams
  { serverParamsControlHost :: URIAuth
  , serverParamsTestSuite :: TestSuiteM ()
  }


type TopicsPending = TVar (Set TestTopic)

-- TODO client threads?
type ServerState = TVar (Map Z.ZMQIdent (TestSuiteState, TopicsPending))

emptyServerState :: STM ServerState
emptyServerState = newTVar Map.empty



startServer :: ServerParams -> IO ()
startServer ServerParams{..} = do
  runZMQ $ do
    server <- Z.socket Router Dealer
    Z.bind server $ T.unpack $ printURI $
      URI (Strict.Just "tcp") True serverParamsControlHost Strict.Nothing [] Strict.Nothing

    serverStateRef <- liftIO (atomically emptyServerState)

    liftIO $ do
      suiteState <- atomically emptyTestSuiteState
      runReaderT serverParamsTestSuite suiteState
      ts <- Map.keysSet <$> atomically (readTVar suiteState)
      when (ts == Set.empty) exitSuccess
    

    forever $ do
      mIncoming <- Z.receive server
      case mIncoming of
        Nothing -> liftIO $ putStrLn $ "No incoming message?"
        Just (addr :: Z.ZMQIdent, incoming :| _) -> case decode (LBS.fromStrict incoming) of
          Nothing -> do
            let err = LBS.toStrict $ encode $ ServerToClientBadParse $ T.decodeUtf8 incoming
            Z.send addr server (err :| [])
            error $ show err
          Just (x :: ClientToServer) -> case x of
            GetTopics -> do
              ts <- liftIO $ registerClient serverParamsTestSuite serverStateRef addr
              Z.sendJson addr server (TopicsAvailable ts)
            ClientToServerBadParse e -> error $ T.unpack e
            Finished t -> do
              liftIO $ putStrLn $ "success: " ++ show t -- FIXME compile report?
              mX <- liftIO $ atomically $ Map.lookup addr <$> readTVar serverStateRef
              case mX of
                Nothing -> error $ "No topic? " ++ show t
                Just (_, pendingTopicsRef) -> do
                  ts <- liftIO $ atomically $ readTVar pendingTopicsRef
                  if ts == Set.empty
                    then liftIO exitSuccess
                    else liftIO $ atomically $ modifyTVar pendingTopicsRef $ Set.delete t
            ClientToServer msg -> case msg of
              -- order:
              -- clientG
              -- serverS
              -- clientD
              -- serverG
              -- clientS
              -- serverD
              -- verify
              GeneratedInput t y -> do
                mSuiteState <- liftIO $ atomically $ getTestSuiteState serverStateRef addr
                case mSuiteState of
                  Nothing -> error "No test suite state!"
                  Just suiteState -> do
                    mOk <- liftIO $ gotClientGenValue suiteState t y
                    if isOkay mOk
                      then do
                        mOutgoing <- liftIO $ serializeValueClientOrigin suiteState t
                        case mOutgoing of
                          HasTopic (HasClientG outgoing) ->
                            Z.sendJson addr server outgoing
                          _ -> error $ show mOutgoing
                      else error $ show mOk
              DeSerialized t y -> do
                mSuiteState <- liftIO $ atomically $ getTestSuiteState serverStateRef addr
                case mSuiteState of
                  Nothing -> error "No test suite state!"
                  Just suiteState -> do
                    mOk <- liftIO $ gotClientDeSerialize suiteState t y
                    if isOkay mOk
                      then do
                        mOutgoing <- liftIO $ generateValue suiteState t
                        case mOutgoing of
                          HasTopic mOutgoing' -> case mOutgoing' of
                            GenValue outgoing ->
                              Z.sendJson addr server outgoing
                            DoneGenerating -> pure ()
                              -- FIXME should never occur - client dictates
                              -- number of quickchecks
                          _ -> error $ show mOutgoing
                      else error $ show mOk
              Serialized t y -> do
                mSuiteState <- liftIO $ atomically $ getTestSuiteState serverStateRef addr
                case mSuiteState of
                  Nothing -> error "No test suite state!"
                  Just suiteState -> do
                    mOk <- liftIO $ gotClientSerialize suiteState t y
                    if isOkay mOk
                      then do
                        mOutgoing <- liftIO $ deserializeValueClientOrigin suiteState t
                        case mOutgoing of
                          HasTopic (HasClientS (DesValue outgoing)) -> do
                            Z.sendJson addr server outgoing
                            -- verify
                            mOk' <- liftIO $ verify suiteState t
                            if isOkay mOk'
                              then Z.sendJson addr server (Continue t)
                              else error $ show mOk'
                          _ -> error $ show mOutgoing
                      else error $ show mOk
              Failure t y -> error $ "Failure: " ++ show t ++ ", " ++ show y -- TODO compile report




getTestSuiteState :: ServerState -> Z.ZMQIdent -> STM (Maybe TestSuiteState)
getTestSuiteState serverState clientKey =
  fmap fst . Map.lookup clientKey <$> readTVar serverState


registerClient :: TestSuiteM ()
               -> ServerState
               -> Z.ZMQIdent
               -> IO (Set TestTopic)
registerClient tests serverState clientKey = do
  ss <- atomically $ readTVar serverState
  case Map.lookup clientKey ss of
    Just _ -> error "Client key already taken"
    Nothing -> do
      suiteState <- atomically $ do
        x <- emptyTestSuiteState
        y <- newTVar Set.empty
        modifyTVar serverState (Map.insert clientKey (x,y))
        pure x
      runReaderT tests suiteState
      Map.keysSet <$> atomically (readTVar suiteState)
