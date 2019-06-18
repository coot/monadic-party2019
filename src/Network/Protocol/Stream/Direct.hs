{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Protocol.Stream.Direct where

import           Network.Protocol.Stream.Client
import           Network.Protocol.Stream.Server


direct :: forall m id chunk a b. Monad m
       => StreamClient m id chunk a
       -> StreamServer m id chunk b
       -> m (a, b)

direct (Request id_ chunkSize mcollect) StreamServer {handleStream} = do
    collect  <- mcollect
    producer <- handleStream id_ chunkSize
    go collect producer
  where
    go :: Collect  m id chunk a
       -> Producer m id chunk b
       -> m (a, b)

    go Collect {handleChunk} (Chunk chunk mproducer) = do
      collect  <- handleChunk chunk
      producer <- mproducer
      go collect producer

    go Collect {handleEndStream} (EndStream mnext) = do
      client <- handleEndStream
      server <- mnext
      direct client server

direct (ClientDone a) StreamServer {handleDone = b} = return (a, b)
