{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , RecordWildCards
  , ScopedTypeVariables
  , DataKinds
  , NamedFieldPuns
  #-}

module Test.Serialization where

import Test.Serialization.Types
  ( TestSuiteM, MsgType (..), ServerToClient (..), TestTopic
  , ClientToServer (..), TestSuiteState, TestTopicState (..), emptyTestSuiteState
  , gotClientGenValue, serializeValueClientOrigin, getTopicState
  , gotClientSerialize, gotClientDeSerialize
  , deserializeValueClientOrigin, verify, generateValue, getTopicState
  , HasTopic (..), DesValue (..), HasClientG (..), GenValue (..)
  , HasClientS (..), isOkay)

import Data.URI (URI (..), printURI)
import Data.URI.Auth (URIAuth)
import Data.UUID (UUID)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString.UTF8 as BS8
import qualified Data.Strict.Maybe as Strict
import Data.List.NonEmpty (NonEmpty (..))
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson (eitherDecode, encode, toJSON)
import Control.Monad (forever, void, when)
import Control.Monad.Reader (runReaderT)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Control (liftBaseWith)
import Control.Concurrent.Async (async, link, cancel)
import Control.Concurrent.STM
  (STM, TVar, newTVar, atomically, modifyTVar, readTVar, writeTVar)
import System.ZMQ4.Monadic
  (ZMQ, runZMQ, Router (..), Dealer (..))
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
        Just (addr :: Z.ZMQIdent, incoming :| _) -> case eitherDecode (LBS.fromStrict incoming) of
          Left e -> do
            let err = LBS.toStrict $ encode $ ServerToClientBadParse $ T.decodeUtf8 incoming
            Z.send addr server (err :| [])
            error $ "Couldn't parse, sending `BadParse`: " ++ BS8.toString incoming ++ ", json error: " ++ e ++ ", original: " ++ BS8.toString incoming
          Right (x :: ClientToServer) -> case x of
            GetTopics -> do
              ts <- liftIO $ registerClient serverParamsTestSuite serverStateRef addr
              () <$ send addr server (TopicsAvailable ts)
            ClientToServerBadParse e -> error $ T.unpack e
            Finished t -> do
              liftIO $ putStrLn $ "success: " ++ show t -- FIXME compile report?
              mX <- liftIO $ atomically $ Map.lookup addr <$> readTVar serverStateRef
              case mX of
                Nothing -> error $ "Received `Finished` from nonexistent topic? " ++ show t
                Just (_, pendingTopicsRef) -> do
                  ts <- liftIO $ atomically $ readTVar pendingTopicsRef
                  if ts == Set.empty || ts == Set.singleton t
                    then liftIO exitSuccess
                    else liftIO $ atomically $ modifyTVar pendingTopicsRef $ Set.delete t
            -- order:
            -- clientG
            -- serverS
            -- clientD
            -- serverG
            -- clientS
            -- serverD
            -- verify
            ClientToServer t m y -> do
              mSuiteState <- liftIO $ atomically $ getTestSuiteState serverStateRef addr
              case mSuiteState of
                Nothing -> error "No test suite state!"
                Just suiteState -> do
                  mState <- liftIO $ getTopicState suiteState t
                  case mState of
                    NoTopic -> error "No topic"
                    HasTopic state -> case m of
                      Failure -> do
                        liftIO $ dumpTopic serverStateRef addr t
                        error $ "Failure: " ++ show t ++ ", " ++ show y -- TODO compile report
                      GeneratedInput -> do
                        liftIO $ atomically $ writeTVar (clientGReceived state) (Just incoming)
                        mOk <- liftIO $ gotClientGenValue state y
                        if isOkay mOk
                          then do
                            mOutgoing <- liftIO $ serializeValueClientOrigin state t
                            case mOutgoing of
                              HasClientG outgoing -> do
                                o' <- send addr server outgoing
                                liftIO $ atomically $ writeTVar (serverSSent state) (Just o')
                              _ -> fail' serverStateRef server "Bad serialize: " addr t mOutgoing
                          else fail' serverStateRef server "Bad got gen: " addr t mOk
                      DeSerialized -> do
                        liftIO $ atomically $ writeTVar (clientDReceived state) (Just incoming)
                        mOk <- liftIO $ gotClientDeSerialize state y
                        if isOkay mOk
                          then do
                            mOutgoing <- liftIO $ generateValue state t
                            case mOutgoing of
                              GenValue outgoing -> do
                                o' <- send addr server outgoing
                                liftIO $ atomically $ writeTVar (serverGSent state) (Just o')
                              DoneGenerating -> pure ()
                                -- FIXME should never occur - client dictates
                                -- number of quickchecks
                          else fail' serverStateRef server "Bad got deserialize: " addr t mOk
                      Serialized -> do
                        liftIO $ atomically $ writeTVar (clientSReceived state) (Just incoming)
                        mOk <- liftIO $ gotClientSerialize state y
                        if isOkay mOk
                          then do
                            mOutgoing <- liftIO $ deserializeValueClientOrigin state t
                            case mOutgoing of
                              HasClientS (DesValue outgoing) -> do
                                o' <- send addr server outgoing
                                liftIO $ atomically $ writeTVar (serverDSent state) (Just o')
                                -- verify
                                mOk' <- liftIO $ verify state
                                if isOkay mOk'
                                  then () <$ send addr server (Continue t)
                                  else fail' serverStateRef server "Bad verify: " addr t mOk'
                              _ -> fail' serverStateRef server "Bad deserialize value: " addr t mOutgoing
                          else fail' serverStateRef server "Bad got serialize: " addr t mOk




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
      suiteState <- atomically emptyTestSuiteState
      runReaderT tests suiteState
      ks <- Map.keysSet <$> atomically (readTVar suiteState)
      y <- atomically $ newTVar ks
      atomically $ modifyTVar serverState (Map.insert clientKey (suiteState,y))
      pure ks


fail' :: Show a
      => ServerState
      -> Z.Socket z Router Dealer Z.Bound
      -> String
      -> Z.ZMQIdent
      -> TestTopic
      -> a
      -> ZMQ z ()
fail' serverStateRef server prefix addr t v = do
  liftIO $ dumpTopic serverStateRef addr t
  _ <- send addr server $ ServerToClient t Failure $ toJSON $ show v
  error $ prefix ++ show t ++ ", " ++ show v


send :: Z.ZMQIdent -> Z.Socket z Router Dealer Z.Bound -> ServerToClient -> ZMQ z ByteString
send addr server x = do
  let x' = LBS.toStrict (encode x)
  Z.send addr server (x' :| [])
  pure x'



dumpTopic :: ServerState
          -> Z.ZMQIdent
          -> TestTopic
          -> IO ()
dumpTopic serverStateRef addr t = do
  mSuiteState <- liftIO $ atomically $ getTestSuiteState serverStateRef addr
  case mSuiteState of
    Nothing -> error "No test suite state!"
    Just suiteState -> do
      mState <- getTopicState suiteState t
      case mState of
        NoTopic -> error $ "No topic in test suite! " ++ show t
        HasTopic (TestTopicState {serialize,clientG,serverS,clientD,serverG,clientS,serverD}) -> do
          mClientG <- atomically (readTVar clientG)
          putStrLn $ "clientG: " ++ show (serialize <$> mClientG)
          mServerS <- atomically (readTVar serverS)
          putStrLn $ "serverS: " ++ show mServerS
          mClientD <- atomically (readTVar clientD)
          putStrLn $ "clientD: " ++ show (serialize <$> mClientD)
          mServerG <- atomically (readTVar serverG)
          putStrLn $ "serverG: " ++ show (serialize <$> mServerG)
          mClientS <- atomically (readTVar clientS)
          putStrLn $ "clientS: " ++ show mClientS
          mServerD <- atomically (readTVar serverD)
          putStrLn $ "serverD: " ++ show (serialize <$> mServerD)
