{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  , GADTs
  , RankNTypes
  , ScopedTypeVariables
  , NamedFieldPuns
  , RecordWildCards
  , ExistentialQuantification
  #-}

module Test.Serialization.Types where

import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.:), (.=), Value (Object, String))
import Data.Aeson.Types (typeMismatch, Parser, parseEither)
import Data.Proxy (Proxy (..))
import Control.Applicative ((<|>))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
  ( TVar, TMVar, newTVar, newEmptyTMVar, atomically, modifyTVar, readTVar
  , putTMVar, tryReadTMVar, tryTakeTMVar)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (Gen, unGen)
import Test.QuickCheck.Random (newQCGen)
import GHC.Generics (Generic)


type TestTopic = Text


data ServerToClientControl
  = TopicsAvailable (Set TestTopic)
  | Ready
  deriving (Eq, Show, Generic)

instance ToJSON ServerToClientControl where
  toJSON x = case x of
    TopicsAvailable xs -> object ["topics" .= Set.toList xs]
    Ready -> String "ready"

instance FromJSON ServerToClientControl where
  parseJSON x = case x of
    Object o -> TopicsAvailable . Set.fromList <$> o .: "topics"
    String s
      | s == "ready" -> pure Ready
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "ServerToClientControl" x


data ClientToServerControl
  = ClientRegister UUID
  | ClientDeRegister
  deriving (Eq, Show, Generic)

instance ToJSON ClientToServerControl where
  toJSON x = case x of
    ClientRegister y -> object ["register" .= y]
    ClientDeRegister -> String "deregister"

instance FromJSON ClientToServerControl where
  parseJSON x = case x of
    Object o -> ClientRegister <$> o .: "register"
    String s
      | s == "deregister" -> pure ClientDeRegister
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "ClientToServerControl" x


data ChannelMsg
  = GeneratedInputBy TestTopic Value
  | SerializedBy TestTopic Value
  | DeSerializedBy TestTopic Value
  | FailureBy TestTopic Value
  deriving (Eq, Show, Generic)

instance ToJSON ChannelMsg where
  toJSON x = case x of
    GeneratedInputBy t y -> object ["topic" .= t, "generated" .= y]
    SerializedBy t y -> object ["topic" .= t, "serialized" .= y]
    DeSerializedBy t y -> object ["topic" .= t, "deserialized" .= y]
    FailureBy t y -> object ["topic" .= t, "failure" .= y]

instance FromJSON ChannelMsg where
  parseJSON x = case x of
    Object o -> do
      let gen = GeneratedInputBy <$> o .: "topic" <*> o .: "generated"
          ser = SerializedBy <$> o .: "topic" <*> o .: "serialized"
          des = DeSerializedBy <$> o .: "topic" <*> o .: "deserialized"
          fai = FailureBy <$> o .: "topic" <*> o .: "failure"
      gen <|> ser <|> des <|> fai
    _ -> typeMismatch "ChannelMsg" x



data TestTopicState =
  forall a
  . ( Arbitrary a
    , ToJSON a
    , FromJSON a
    , Eq a
    ) => TestTopicState
  { generate :: Gen a
  , serialize :: a -> Value
  , deserialize :: Value -> Parser a
  , size    :: TVar Int
  , serverG :: TMVar a
  , clientS :: TMVar Value
  , serverD :: TMVar a
  , clientG :: TMVar a
  , serverS :: TMVar Value
  , clientD :: TMVar a
  }

type TestSuiteState = TVar (Map TestTopic TestTopicState)

type TestSuiteM a = ReaderT TestSuiteState IO a

registerTopic :: forall a
               . ( Arbitrary a
                 , ToJSON a
                 , FromJSON a
                 , Eq a
                 ) => TestTopic
                   -> Proxy a
                   -> TestSuiteM ()
registerTopic topic Proxy = do
  xsRef <- ask
  liftIO $ atomically $ do
    size <- newTVar 1
    (serverG :: TMVar a) <- newEmptyTMVar
    clientS <- newEmptyTMVar
    (serverD :: TMVar a) <- newEmptyTMVar
    (clientG :: TMVar a) <- newEmptyTMVar
    serverS <- newEmptyTMVar
    (clientD :: TMVar a) <- newEmptyTMVar
    modifyTVar xsRef $ Map.insert topic TestTopicState
      { size
      , serverG
      , clientS
      , serverD
      , clientG
      , serverS
      , clientD
      , generate = arbitrary
      , serialize = toJSON
      , deserialize = parseJSON
      }


data HasTopic a
  = HasTopic a
  | NoTopic


data GenValue a
  = DoneGenerating
  | GenValue a


data GotClientGenValue a
  = NoClientGenValue
  | GotClientGenValue a


data HasClientG a
  = NoClientG
  | HasClientG a


data HasServerG a
  = NoServerG
  | HasServerG a


data HasServerS a
  = NoServerS
  | HasServerS a


data HasServerD a
  = NoServerD
  | HasServerD a


data HasClientD a
  = NoClientD
  | HasClientD a


data DesValue a
  = CantDes String
  | DesValue a


data HasClientS a
  = NoClientS
  | HasClientS a


getTopicState :: TestSuiteState
              -> TestTopic
              -> IO (HasTopic TestTopicState)
getTopicState xsRef topic = do
  xs <- atomically (readTVar xsRef)
  case Map.lookup topic xs of
    Nothing -> pure NoTopic
    Just x -> pure (HasTopic x)


generateValue :: TestSuiteState
              -> TestTopic
              -> IO (HasTopic (GenValue ()))
generateValue xsRef topic = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic (TestTopicState{size,generate,serialize,serverG}) -> fmap HasTopic $ do
      s <- atomically (readTVar size)
      if s >= 100
        then pure DoneGenerating
        else do
          g <- newQCGen
          let val = unGen generate g s
          atomically $ do
            modifyTVar size (+ 1)
            putTMVar serverG val
          pure (GenValue ())


gotClientGenValue :: TestSuiteState
                  -> TestTopic
                  -> Value
                  -> IO (HasTopic (DesValue ()))
gotClientGenValue xsRef topic value = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic (TestTopicState{deserialize,clientG}) -> fmap HasTopic $ do
      case parseEither deserialize value of
        Left e -> pure (CantDes e)
        Right y -> do
          atomically (putTMVar clientG y)
          pure (DesValue ())


serializeValueClientOrigin :: TestSuiteState
                           -> TestTopic
                           -> IO (HasTopic (HasClientG ()))
serializeValueClientOrigin xsRef topic = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic (TestTopicState{serialize,clientG,serverS}) -> fmap HasTopic $ do
      mX <- atomically (tryReadTMVar clientG)
      case mX of
        Nothing -> pure NoClientG
        Just x -> fmap HasClientG $ do
          atomically $ putTMVar serverS $ serialize x
          pure ()


gotClientSerialize :: TestSuiteState
                   -> TestTopic
                   -> Value
                   -> IO (HasTopic ())
gotClientSerialize xsRef topic value = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic (TestTopicState{deserialize,clientS}) -> fmap HasTopic $ do
      atomically (putTMVar clientS value)
      pure ()


deserializeValueClientOrigin :: TestSuiteState
                             -> TestTopic
                             -> IO (HasTopic (HasClientS (DesValue ())))
deserializeValueClientOrigin xsRef topic = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic (TestTopicState{deserialize,clientS,serverD}) -> fmap HasTopic $ do
      mX <- atomically (tryReadTMVar clientS)
      case mX of
        Nothing -> pure NoClientS
        Just x -> fmap HasClientS $ case parseEither deserialize x of
          Left e -> pure (CantDes e)
          Right y -> do
            atomically (putTMVar serverD y)
            pure (DesValue ())


gotClientDeSerialize :: TestSuiteState
                     -> TestTopic
                     -> Value
                     -> IO (HasTopic (DesValue ()))
gotClientDeSerialize xsRef topic value = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic (TestTopicState{deserialize,clientD}) -> fmap HasTopic $ do
      case parseEither deserialize value of
        Left e -> pure (CantDes e)
        Right y -> do
          atomically (putTMVar clientD y)
          pure (DesValue ())


data ServerSerializedMatch a
  = ServerSerializedMatch a
  | ServerSerializedMismatch


data ServerDeSerializedMatch a
  = ServerDeSerializedMatch a
  | ServerDeSerializedMismatch


data ClientSerializedMatch a
  = ClientSerializedMatch a
  | ClientSerializedMismatch


data ClientDeSerializedMatch a
  = ClientDeSerializedMatch a
  | ClientDeSerializedMismatch


verify :: TestSuiteState
       -> TestTopic
       -> IO
          ( HasTopic
            ( HasServerG
              ( HasClientS
                ( ServerSerializedMatch
                  ( HasServerD
                    ( DesValue
                      ( ServerDeSerializedMatch
                        ( HasClientG
                          ( HasServerS
                            ( ClientSerializedMatch
                              ( HasClientD
                                ( DesValue
                                  ( ClientDeSerializedMatch ())))))))))))))
verify xsRef topic = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic TestTopicState{..} -> fmap HasTopic $ do
      mServerG <- atomically (tryTakeTMVar serverG)
      case mServerG of
        Nothing -> pure NoServerG
        Just serverG' -> fmap HasServerG $ do
          mClientS <- atomically (tryTakeTMVar clientS)
          case mClientS of
            Nothing -> pure NoClientS
            Just clientS' -> fmap HasClientS $ do
              if serialize serverG' /= clientS'
                then pure ServerSerializedMismatch
                else fmap ServerSerializedMatch $ do
                  mServerD <- atomically (tryTakeTMVar serverD)
                  case mServerD of
                    Nothing -> pure NoServerD
                    Just serverD' -> fmap HasServerD $ do
                      case parseEither deserialize clientS' of
                        Left e -> pure (CantDes e)
                        Right serverD''
                          | serverD'' /= serverD' -> pure (DesValue ServerDeSerializedMismatch)
                          | otherwise -> fmap (DesValue . ServerDeSerializedMatch) $ do
                              mClientG <- atomically (tryTakeTMVar clientG)
                              case mClientG of
                                Nothing -> pure NoClientG
                                Just clientG' -> fmap HasClientG $ do
                                  mServerS <- atomically (tryTakeTMVar serverS)
                                  case mServerS of
                                    Nothing -> pure NoServerS
                                    Just serverS' -> fmap HasServerS $ do
                                      if serialize clientG' /= serverS'
                                        then pure ClientSerializedMismatch
                                        else fmap ClientSerializedMatch $ do
                                          mClientD <- atomically (tryTakeTMVar clientD)
                                          case mClientD of
                                            Nothing -> pure NoClientD
                                            Just clientD' -> fmap HasClientD $ do
                                              case parseEither deserialize serverS' of
                                                Left e -> pure (CantDes e)
                                                Right serverS''
                                                  | serverS'' /= clientD' -> pure (DesValue ClientDeSerializedMismatch)
                                                  | otherwise -> fmap (DesValue . ClientDeSerializedMatch) $ pure ()

