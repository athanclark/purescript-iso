{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  , GADTs
  , RankNTypes
  , ScopedTypeVariables
  , NamedFieldPuns
  , RecordWildCards
  , ExistentialQuantification
  , GeneralizedNewtypeDeriving
  #-}

module Test.Serialization.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.UUID (UUID)
import Data.Aeson
  (FromJSON (..), ToJSON (..), object, (.:), (.=), Value (..))
import Data.Aeson.Types (typeMismatch, Parser, parseEither)
import Data.Proxy (Proxy (..))
import Data.String (IsString)
import Control.Applicative ((<|>))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
  ( STM, TVar, TMVar, newTVar, newEmptyTMVar, atomically, modifyTVar, readTVar
  , putTMVar, tryReadTMVar, tryTakeTMVar)
import Test.QuickCheck (Arbitrary (..))
import Test.QuickCheck.Gen (Gen, unGen, oneof, listOf1, elements, scale)
import Test.QuickCheck.Random (newQCGen)
import Test.QuickCheck.Instances ()
import GHC.Generics (Generic)


newtype TestTopic = TestTopic Text
  deriving (IsString, Eq, Ord, Generic, Show, ToJSON, FromJSON)

instance Arbitrary TestTopic where
  arbitrary = TestTopic . T.pack <$> listOf1 (elements ['a' .. 'z'])


instance Arbitrary Value where
  arbitrary =
    oneof
      [ termNull
      , termNumber
      , termBool
      , termString
      , scale (`div` 2) (Array <$> arbitrary)
      , scale (`div` 2) (Object <$> arbitrary)
      ]
    where
      termNull = pure Null
      termNumber = Number <$> arbitrary
      termBool = Bool <$> arbitrary
      termString = String <$> arbitraryNonEmptyText
      arbitraryNonEmptyText = T.pack <$> listOf1 (elements ['a' .. 'z'])



data ChannelMsg
  = GeneratedInput TestTopic Value
  | Serialized TestTopic Value
  | DeSerialized TestTopic Value
  | Failure TestTopic Value
  deriving (Eq, Show, Generic)

instance Arbitrary ChannelMsg where
  arbitrary = oneof
    [ GeneratedInput <$> arbitrary <*> arbitrary -- Json
    , Serialized <$> arbitrary <*> arbitrary -- Json
    , DeSerialized <$> arbitrary <*> arbitrary -- Json
    , Failure <$> arbitrary <*> arbitrary -- Json
    ]
    -- where
    --   arbitraryJson = pure (String "foo")

instance ToJSON ChannelMsg where
  toJSON x = case x of
    GeneratedInput t y -> object ["topic" .= t, "generated" .= y]
    Serialized t y -> object ["topic" .= t, "serialized" .= y]
    DeSerialized t y -> object ["topic" .= t, "deserialized" .= y]
    Failure t y -> object ["topic" .= t, "failure" .= y]

instance FromJSON ChannelMsg where
  parseJSON x = case x of
    Object o -> do
      let gen = GeneratedInput <$> o .: "topic" <*> o .: "generated"
          ser = Serialized <$> o .: "topic" <*> o .: "serialized"
          des = DeSerialized <$> o .: "topic" <*> o .: "deserialized"
          fai = Failure <$> o .: "topic" <*> o .: "failure"
      gen <|> ser <|> des <|> fai
    _ -> typeMismatch "ChannelMsg" x


data ClientToServer
  = GetTopics
  | ClientToServer ChannelMsg
  | ClientToServerBadParse Text
  | Finished TestTopic
  deriving (Eq, Show, Generic)

instance Arbitrary ClientToServer where
  arbitrary = oneof
    [ pure GetTopics
    , ClientToServer <$> arbitrary
    , ClientToServerBadParse <$> arbitraryNonEmptyText
    , Finished <$> arbitrary
    ]
    where
      arbitraryNonEmptyText = T.pack <$> listOf1 (elements ['a' .. 'z'])

instance ToJSON ClientToServer where
  toJSON x = case x of
    GetTopics -> String "getTopics"
    ClientToServer y -> object ["channelMsg" .= y]
    ClientToServerBadParse y -> object ["badParse" .= y]
    Finished y -> object ["finished" .= y]

instance FromJSON ClientToServer where
  parseJSON json = case json of
    Object o -> do
      let chn = ClientToServer <$> o .: "channelMsg"
          bd = ClientToServerBadParse <$> o .: "badParse"
          fin = Finished <$> o .: "finished"
      chn <|> bd <|> fin
    String s
      | s == "getTopics" -> pure GetTopics
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "ClientToServer" json


data ServerToClient
  = TopicsAvailable (Set TestTopic)
  | ServerToClient ChannelMsg
  | ServerToClientBadParse Text
  | Continue TestTopic
  deriving (Eq, Show, Generic)

instance Arbitrary ServerToClient where
  arbitrary = oneof
    [ TopicsAvailable <$> arbitrary
    , ServerToClient <$> arbitrary
    , ServerToClientBadParse <$> arbitraryNonEmptyText
    , Continue <$> arbitrary
    ]
    where
      arbitraryNonEmptyText = T.pack <$> listOf1 (elements ['a' .. 'z'])

instance ToJSON ServerToClient where
  toJSON x = case x of
    TopicsAvailable xs -> object ["topics" .= Set.toList xs]
    ServerToClient y -> object ["channelMsg" .= y]
    ServerToClientBadParse x -> object ["badParse" .= x]
    Continue y -> object ["continue" .= y]

instance FromJSON ServerToClient where
  parseJSON x = case x of
    Object o -> do
      let available = TopicsAvailable . Set.fromList <$> o .: "topics"
          badParse = ServerToClientBadParse <$> o .: "badParse"
          channel = ServerToClient <$> o .: "channelMsg"
          continue = Continue <$> o .: "continue"
      available <|> badParse <|> channel <|> continue
    _ -> fail'
    where
      fail' = typeMismatch "ServerToClientControl" x



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

emptyTestTopicState :: forall a
                     . ( Arbitrary a
                       , ToJSON a
                       , FromJSON a
                       , Eq a
                       ) => Proxy a -> STM TestTopicState
emptyTestTopicState Proxy = do
  size <- newTVar 1
  (serverG :: TMVar a) <- newEmptyTMVar
  clientS <- newEmptyTMVar
  (serverD :: TMVar a) <- newEmptyTMVar
  (clientG :: TMVar a) <- newEmptyTMVar
  serverS <- newEmptyTMVar
  (clientD :: TMVar a) <- newEmptyTMVar
  pure TestTopicState
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


type TestSuiteState = TVar (Map TestTopic TestTopicState)

emptyTestSuiteState :: STM TestSuiteState
emptyTestSuiteState = newTVar Map.empty


type TestSuiteM a = ReaderT TestSuiteState IO a


registerTopic :: forall a
               . ( Arbitrary a
                 , ToJSON a
                 , FromJSON a
                 , Eq a
                 ) => TestTopic
                   -> Proxy a
                   -> TestSuiteM ()
registerTopic topic p = do
  xsRef <- ask
  liftIO $ atomically $ do
    state <- emptyTestTopicState p
    modifyTVar xsRef (Map.insert topic state)



class IsOkay a where
  isOkay :: a -> Bool

instance IsOkay () where
  isOkay () = True

data HasTopic a
  = HasTopic a
  | NoTopic
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (HasTopic a) where
  isOkay x = case x of
    NoTopic -> False
    HasTopic y -> isOkay y


data GenValue a
  = DoneGenerating
  | GenValue a
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (GenValue a) where
  isOkay x = case x of
    DoneGenerating -> False
    GenValue y -> isOkay y


data GotClientGenValue a
  = NoClientGenValue
  | GotClientGenValue a
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (GotClientGenValue a) where
  isOkay x = case x of
    NoClientGenValue -> False
    GotClientGenValue y -> isOkay y


data HasClientG a
  = NoClientG
  | HasClientG a
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (HasClientG a) where
  isOkay x = case x of
    NoClientG -> False
    HasClientG y -> isOkay y


data HasServerG a
  = NoServerG
  | HasServerG a
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (HasServerG a) where
  isOkay x = case x of
    NoServerG -> False
    HasServerG y -> isOkay y


data HasServerS a
  = NoServerS
  | HasServerS a
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (HasServerS a) where
  isOkay x = case x of
    NoServerS -> False
    HasServerS y -> isOkay y


data HasServerD a
  = NoServerD
  | HasServerD a
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (HasServerD a) where
  isOkay x = case x of
    NoServerD -> False
    HasServerD y -> isOkay y


data HasClientD a
  = NoClientD
  | HasClientD a
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (HasClientD a) where
  isOkay x = case x of
    NoClientD -> False
    HasClientD y -> isOkay y


data DesValue a
  = CantDes String
  | DesValue a
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (DesValue a) where
  isOkay x = case x of
    CantDes _ -> False
    DesValue y -> isOkay y


data HasClientS a
  = NoClientS
  | HasClientS a
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (HasClientS a) where
  isOkay x = case x of
    NoClientS -> False
    HasClientS y -> isOkay y


data ServerSerializedMatch a
  = ServerSerializedMatch a
  | ServerSerializedMismatch
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (ServerSerializedMatch a) where
  isOkay x = case x of
    ServerSerializedMismatch -> False
    ServerSerializedMatch y -> isOkay y


data ServerDeSerializedMatch a
  = ServerDeSerializedMatch a
  | ServerDeSerializedMismatch
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (ServerDeSerializedMatch a) where
  isOkay x = case x of
    ServerDeSerializedMismatch -> False
    ServerDeSerializedMatch y -> isOkay y


data ClientSerializedMatch a
  = ClientSerializedMatch a
  | ClientSerializedMismatch
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (ClientSerializedMatch a) where
  isOkay x = case x of
    ClientSerializedMismatch -> False
    ClientSerializedMatch y -> isOkay y


data ClientDeSerializedMatch a
  = ClientDeSerializedMatch a
  | ClientDeSerializedMismatch
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (ClientDeSerializedMatch a) where
  isOkay x = case x of
    ClientDeSerializedMismatch -> False
    ClientDeSerializedMatch y -> isOkay y


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
              -> IO (HasTopic (GenValue ChannelMsg))
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
          pure $ GenValue $ GeneratedInput topic $ serialize val


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
                           -> IO (HasTopic (HasClientG ChannelMsg))
serializeValueClientOrigin xsRef topic = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic (TestTopicState{serialize,clientG,serverS}) -> fmap HasTopic $ do
      mX <- atomically (tryReadTMVar clientG)
      case mX of
        Nothing -> pure NoClientG
        Just x -> fmap HasClientG $ do
          let val = serialize x
          atomically $ putTMVar serverS val
          pure $ Serialized topic val


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
                             -> IO (HasTopic (HasClientS (DesValue ChannelMsg)))
deserializeValueClientOrigin xsRef topic = do
  mState <- getTopicState xsRef topic
  case mState of
    NoTopic -> pure NoTopic
    HasTopic (TestTopicState{deserialize,clientS,serverD,serialize}) -> fmap HasTopic $ do
      mX <- atomically (tryReadTMVar clientS)
      case mX of
        Nothing -> pure NoClientS
        Just x -> fmap HasClientS $ case parseEither deserialize x of
          Left e -> pure (CantDes e)
          Right y -> do
            atomically (putTMVar serverD y)
            pure $ DesValue $ DeSerialized topic $ serialize y


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

