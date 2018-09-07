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
  , PartialTypeSignatures
  #-}

module Test.Serialization.Types where

import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as LBS
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Aeson
  (FromJSON (..), ToJSON (..), object, (.:), (.=), Value (..), encode, decode)
import Data.Aeson.Types (typeMismatch, Parser, parseEither)
import Data.Aeson.Diff (diff)
import Data.Proxy (Proxy (..))
import Data.String (IsString)
import Control.Applicative ((<|>))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
  ( STM, TVar, newTVar, atomically, modifyTVar, readTVar
  , writeTVar)
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



data MsgType
  = GeneratedInput
  | Serialized
  | DeSerialized
  | Failure
  deriving (Eq, Show, Generic)

instance Arbitrary MsgType where
  arbitrary = oneof
    [ pure GeneratedInput
    , pure Serialized
    , pure DeSerialized
    , pure Failure
    ]

instance ToJSON MsgType where
  toJSON x = String $ case x of
    GeneratedInput -> "generated"
    Serialized -> "serialized"
    DeSerialized -> "deserialized"
    Failure -> "failure"

instance FromJSON MsgType where
  parseJSON x = case x of
    String s
      | s == "generated" -> pure GeneratedInput
      | s == "serialized" -> pure Serialized
      | s == "deserialized" -> pure DeSerialized
      | s == "failure" -> pure Failure
      | otherwise -> fail'
    _ -> fail'
    where
      fail' = typeMismatch "MsgType" x



data ClientToServer
  = GetTopics
  | ClientToServer TestTopic MsgType Value
  | ClientToServerBadParse Text
  | Finished TestTopic
  deriving (Eq, Show, Generic)

instance Arbitrary ClientToServer where
  arbitrary = oneof
    [ pure GetTopics
    , ClientToServer <$> arbitrary <*> arbitrary <*> arbitrary
    , ClientToServerBadParse <$> arbitraryNonEmptyText
    , Finished <$> arbitrary
    ]
    where
      arbitraryNonEmptyText = T.pack <$> listOf1 (elements ['a' .. 'z'])

instance ToJSON ClientToServer where
  toJSON x = case x of
    GetTopics -> String "getTopics"
    ClientToServer t m y -> object ["topic" .= t, "msgType" .= m, "value" .= y]
    ClientToServerBadParse y -> object ["badParse" .= y]
    Finished y -> object ["finished" .= y]

instance FromJSON ClientToServer where
  parseJSON json = case json of
    Object o -> do
      let chn = ClientToServer <$> o .: "topic" <*> o .: "msgType" <*> o .: "value"
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
  | ServerToClient TestTopic MsgType Value
  | ServerToClientBadParse Text
  | Continue TestTopic
  deriving (Eq, Show, Generic)

instance Arbitrary ServerToClient where
  arbitrary = oneof
    [ TopicsAvailable <$> arbitrary
    , ServerToClient <$> arbitrary <*> arbitrary <*> arbitrary
    , ServerToClientBadParse <$> arbitraryNonEmptyText
    , Continue <$> arbitrary
    ]
    where
      arbitraryNonEmptyText = T.pack <$> listOf1 (elements ['a' .. 'z'])

instance ToJSON ServerToClient where
  toJSON x = case x of
    TopicsAvailable xs -> object ["topics" .= Set.toList xs]
    ServerToClient t m y -> object ["topic" .= t, "msgType" .= m, "value" .= y]
    ServerToClientBadParse y -> object ["badParse" .= y]
    Continue y -> object ["continue" .= y]

instance FromJSON ServerToClient where
  parseJSON x = case x of
    Object o -> do
      let available = TopicsAvailable . Set.fromList <$> o .: "topics"
          badParse = ServerToClientBadParse <$> o .: "badParse"
          channel = ServerToClient <$> o .: "topic" <*> o .: "msgType" <*> o .: "value"
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
  , serverG :: TVar (Maybe a)
  , serverGSent :: TVar (Maybe ByteString)
  , clientS :: TVar (Maybe Value)
  , clientSReceived :: TVar (Maybe ByteString)
  , serverD :: TVar (Maybe a)
  , serverDSent :: TVar (Maybe ByteString)
  , clientG :: TVar (Maybe a)
  , clientGReceived :: TVar (Maybe ByteString)
  , serverS :: TVar (Maybe Value)
  , serverSSent :: TVar (Maybe ByteString)
  , clientD :: TVar (Maybe a)
  , clientDReceived :: TVar (Maybe ByteString)
  }

emptyTestTopicState :: forall a
                     . ( Arbitrary a
                       , ToJSON a
                       , FromJSON a
                       , Eq a
                       ) => Proxy a -> STM TestTopicState
emptyTestTopicState Proxy = do
  size <- newTVar 1
  (serverG :: TVar (Maybe a)) <- newTVar Nothing
  serverGSent <- newTVar Nothing
  clientS <- newTVar Nothing
  clientSReceived <- newTVar Nothing
  (serverD :: TVar (Maybe a)) <- newTVar Nothing
  serverDSent <- newTVar Nothing
  (clientG :: TVar (Maybe a)) <- newTVar Nothing
  clientGReceived <- newTVar Nothing
  serverS <- newTVar Nothing
  serverSSent <- newTVar Nothing
  (clientD :: TVar (Maybe a)) <- newTVar Nothing
  clientDReceived <- newTVar Nothing
  pure TestTopicState
    { size
    , serverG
    , serverGSent
    , clientS
    , clientSReceived
    , serverD
    , serverDSent
    , clientG
    , clientGReceived
    , serverS
    , serverSSent
    , clientD
    , clientDReceived
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
    { serverSerializedServerG :: Value
    , serverSerializedClientS :: Value
    , serverSerializedServerGSent :: LBS.ByteString
    , serverSerializedClientSReceived :: LBS.ByteString
    }
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (ServerSerializedMatch a) where
  isOkay x = case x of
    ServerSerializedMismatch _ _ _ _ -> False
    ServerSerializedMatch y -> isOkay y


data ServerDeSerializedMatch a
  = ServerDeSerializedMatch a
  | ServerDeSerializedMismatch
    { serverDeSerializedClientS :: Value
    , serverDeSerializedServerD :: Value
    , serverDeSerializedClientSReceived :: LBS.ByteString
    , serverDeSerializedServerDSent :: LBS.ByteString
    }
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (ServerDeSerializedMatch a) where
  isOkay x = case x of
    ServerDeSerializedMismatch _ _ _ _ -> False
    ServerDeSerializedMatch y -> isOkay y


data ClientSerializedMatch a
  = ClientSerializedMatch a
  | ClientSerializedMismatch
    { clientSerializedClientG :: Value
    , clientSerializedServerS :: Value
    , clientSerializedClientGReceived :: LBS.ByteString
    , clientSerializedServerSSent :: LBS.ByteString
    }
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (ClientSerializedMatch a) where
  isOkay x = case x of
    ClientSerializedMismatch _ _ _ _ -> False
    ClientSerializedMatch y -> isOkay y


data ClientDeSerializedMatch a
  = ClientDeSerializedMatch a
  | ClientDeSerializedMismatch
    { clientDeSerializedServerS :: Value
    , clientDeSerializedClientD :: Value
    , clientDeSerializedServerSSent :: LBS.ByteString
    , clientDeSerializedClientDReceived :: LBS.ByteString
    }
  deriving (Eq, Show, Generic)

instance IsOkay a => IsOkay (ClientDeSerializedMatch a) where
  isOkay x = case x of
    ClientDeSerializedMismatch _ _ _ _ -> False
    ClientDeSerializedMatch y -> isOkay y


getTopicState :: TestSuiteState
              -> TestTopic
              -> IO (HasTopic TestTopicState)
getTopicState xsRef topic = do
  xs <- atomically (readTVar xsRef)
  case Map.lookup topic xs of
    Nothing -> pure NoTopic
    Just x -> pure (HasTopic x)


generateValue :: TestTopicState
              -> TestTopic
              -> Int
              -> IO (GenValue ServerToClient)
generateValue TestTopicState{size,generate,serialize,serverG} topic maxSize = do
  s <- atomically (readTVar size)
  if s >= maxSize
    then pure DoneGenerating
    else do
      g <- newQCGen
      let val = unGen generate g s
      atomically $ do
        modifyTVar size (+ 1)
        writeTVar serverG $ Just val
      pure $ GenValue $ ServerToClient topic GeneratedInput $ serialize val


gotClientGenValue :: TestTopicState
                  -> Value
                  -> IO (DesValue ())
gotClientGenValue TestTopicState{deserialize,clientG} value = do
  case parseEither deserialize value of
    Left e -> pure (CantDes e)
    Right y -> do
      atomically $ writeTVar clientG $ Just y
      pure (DesValue ())


serializeValueClientOrigin :: TestTopicState
                           -> TestTopic
                           -> IO (HasClientG ServerToClient)
serializeValueClientOrigin TestTopicState{serialize,clientG,serverS} topic = do
  mX <- atomically (readTVar clientG)
  case mX of
    Nothing -> pure NoClientG
    Just x -> fmap HasClientG $ do
      let val = serialize x
      atomically $ writeTVar serverS $ Just val
      pure $ ServerToClient topic Serialized val


gotClientSerialize :: TestTopicState
                   -> Value
                   -> IO ()
gotClientSerialize TestTopicState{clientS} value = do
  atomically $ writeTVar clientS $ Just value


deserializeValueClientOrigin :: TestTopicState
                             -> TestTopic
                             -> IO (HasClientS (DesValue ServerToClient))
deserializeValueClientOrigin TestTopicState{deserialize,clientS,serverD,serialize} topic = do
  mX <- atomically (readTVar clientS)
  case mX of
    Nothing -> pure NoClientS
    Just x -> fmap HasClientS $ case parseEither deserialize x of
      Left e -> pure (CantDes e)
      Right y -> do
        atomically $ writeTVar serverD $ Just y
        pure $ DesValue $ ServerToClient topic DeSerialized $ serialize y


gotClientDeSerialize :: TestTopicState
                     -> Value
                     -> IO (DesValue ())
gotClientDeSerialize TestTopicState{deserialize,clientD} value = do
  case parseEither deserialize value of
    Left e -> pure (CantDes e)
    Right y -> do
      atomically $ writeTVar clientD $ Just y
      pure (DesValue ())


verify :: TestTopicState
       -> IO
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
                                ( ClientDeSerializedMatch ()))))))))))))
verify
  TestTopicState
  { serverG
  , clientS
  , serverD
  , clientG
  , serverS
  , clientD
  , deserialize
  , serialize
  } = do
  let serverSMatch :: (Value -> IO _) -> IO (HasServerG (HasClientS (ServerSerializedMatch _)))
      serverSMatch x = do
        mServerG <- atomically (readTVar serverG)
        case mServerG of
          Nothing -> pure NoServerG
          Just serverG' -> fmap HasServerG $ do
            mClientS <- atomically (readTVar clientS)
            case mClientS of
              Nothing -> pure NoClientS
              Just clientS' -> fmap HasClientS $ do
                let serverG'' = serialize serverG'
                if serverG'' /= clientS'
                  then do
                    putStrLn $ "Diff: " ++ show (diff serverG'' clientS')
                    pure $ ServerSerializedMismatch serverG'' clientS' (encode serverG'') (encode clientS')
                  else ServerSerializedMatch <$> x clientS'
      serverDMatch :: IO _ -> Value -> IO (HasServerD (DesValue (ServerDeSerializedMatch _)))
      serverDMatch x clientS' = do
        mServerD <- atomically (readTVar serverD)
        case mServerD of
          Nothing -> pure NoServerD
          Just serverD' -> fmap HasServerD $
            case parseEither deserialize clientS' of
              Left e -> pure (CantDes e)
              Right clientS''
                | clientS'' /= serverD' -> pure $ DesValue $ ServerDeSerializedMismatch clientS' (toJSON serverD') (encode clientS') (encode serverD')
                | otherwise -> (DesValue . ServerDeSerializedMatch) <$> x
      clientSMatch :: (Value -> IO _) -> IO (HasClientG (HasServerS (ClientSerializedMatch _)))
      clientSMatch x = do
        mClientG <- atomically (readTVar clientG)
        case mClientG of
          Nothing -> pure NoClientG
          Just clientG' -> fmap HasClientG $ do
            mServerS <- atomically (readTVar serverS)
            case mServerS of
              Nothing -> pure NoServerS
              Just serverS' -> fmap HasServerS $ do
                let clientG'' = serialize clientG'
                if clientG'' /= serverS'
                  then do
                    putStrLn $ "Diff: " ++ show (diff clientG'' serverS')
                    pure $ ClientSerializedMismatch clientG'' serverS' (encode clientG'') (encode serverS')
                  else ClientSerializedMatch <$> x serverS'
      clientDMatch :: Value -> IO (HasClientD (DesValue (ClientDeSerializedMatch ())))
      clientDMatch serverS' = do
        mClientD <- atomically (readTVar clientD)
        case mClientD of
          Nothing -> pure NoClientD
          Just clientD' -> fmap HasClientD $
            case parseEither deserialize serverS' of
              Left e -> pure (CantDes e)
              Right serverS''
                | serverS'' /= clientD' -> pure $ DesValue $ ClientDeSerializedMismatch serverS' (toJSON clientD') (encode serverS') (encode clientD')
                | otherwise -> pure $ DesValue $ ClientDeSerializedMatch ()
  serverSMatch $ serverDMatch $ clientSMatch clientDMatch

