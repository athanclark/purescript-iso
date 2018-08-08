{-# LANGUAGE
    OverloadedStrings
  , OverloadedLists
  , RecordWildCards
  #-}

module Test.Serialization where

import Data.URI (URI (..), printURI)
import Data.URI.Auth (URIAuth)
import qualified Data.Text as T
import qualified Data.Strict.Maybe as Strict
import System.ZMQ4.Monadic
  (runZMQ, socket, bind, Rep (..), Pub (..))


data ServerParams = ServerParams
  { serverParamsControlHost :: URIAuth
  , serverParamsChannelHost :: URIAuth
  -- , serverParamsTestSuite :: Compiled Test Suite with Topic Names etc
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
