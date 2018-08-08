{-# LANGUAGE
    DeriveGeneric
  , OverloadedStrings
  #-}

module Test.Serialization.Types where

import Data.Text (Text)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.UUID (UUID)
import Data.Aeson (FromJSON (..), ToJSON (..), object, (.:), (.=), Value (Object, String))
import Data.Aeson.Types (typeMismatch)
import Control.Applicative ((<|>))
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

data ServerToClientChannel a
  = GeneratedInputByServer a
  | SerializedByServer Value
  | DeSerializedByServer a
  deriving (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (ServerToClientChannel a) where
  toJSON x = case x of
    GeneratedInputByServer y -> object ["generated" .= y]
    SerializedByServer y -> object ["serialized" .= y]
    DeSerializedByServer y -> object ["deserialized" .= y]

instance (FromJSON a) => FromJSON (ServerToClientChannel a) where
  parseJSON x = case x of
    Object o -> do
      let gen = GeneratedInputByServer <$> o .: "generated"
          ser = SerializedByServer <$> o .: "serialized"
          des = DeSerializedByServer <$> o .: "deserialized"
      gen <|> ser <|> des
    _ -> typeMismatch "ServerToClientChannel" x


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


data ClientToServerChannel a
  = GeneratedInputByClient a
  | SerializedByClient Value
  | DeSerializedByClient a
  deriving (Eq, Show, Generic)

instance (ToJSON a) => ToJSON (ClientToServerChannel a) where
  toJSON x = case x of
    GeneratedInputByClient y -> object ["generated" .= y]
    SerializedByClient y -> object ["serialized" .= y]
    DeSerializedByClient y -> object ["deserialized" .= y]

instance (FromJSON a) => FromJSON (ClientToServerChannel a) where
  parseJSON x = case x of
    Object o -> do
      let gen = GeneratedInputByClient <$> o .: "generated"
          ser = SerializedByClient <$> o .: "serialized"
          des = DeSerializedByClient <$> o .: "deserialized"
      gen <|> ser <|> des
    _ -> typeMismatch "ClientToServerChannel" x
