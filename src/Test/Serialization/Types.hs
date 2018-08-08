{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  , GADTs
  , RankNTypes
  , ScopedTypeVariables
  , NamedFieldPuns
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
import Data.Aeson.Types (typeMismatch)
import Data.Proxy (Proxy (..))
import Control.Applicative ((<|>))
import Control.Monad.Reader (ReaderT, ask)
import Control.Monad.IO.Class (liftIO)
import Control.Concurrent.STM
  (TVar, TMVar, newTVar, newEmptyTMVar, atomically, modifyTVar)
import Test.QuickCheck (Arbitrary)
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
    ) => TestTopicState
  { size    :: TVar Int
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
      {size,serverG,clientS,serverD,clientG,serverS,clientD}


-- type SerializeTestsM 
