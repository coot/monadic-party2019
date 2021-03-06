{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Protocol.Stream.Server where


import           Network.TypedProtocol.Core

import           Network.Protocol.Stream.Type


newtype StreamServer m id chunk a = StreamServer {
    handleStream :: id
                 -- ^ resouce id
                 -> Int
                 -- ^ chunk size
                 -> m (Producer m chunk a)
  }


data Producer m chunk a where
     Chunk
       :: chunk
       -> m (Producer m chunk a)
       -> Producer m chunk a

     EndStream
       :: m a
       -> Producer m chunk a


streamServerPeer
    :: forall m id chunk a.
       Monad m
    => StreamServer m id chunk a
    -> Peer (Stream id chunk) 'AsServer 'StIdle m a
streamServerPeer StreamServer {handleStream} =
    Await (ClientAgency TokIdle) $ \(MsgGet chunkSize id_) ->
      Effect $ handleStream id_ chunkSize >>= pure . producer
  where
    producer :: Producer m chunk a
             -> Peer (Stream id chunk) 'AsServer 'StBusy m a

    producer (Chunk chunk mnext) =
      Yield (ServerAgency TokBusy) (MsgChunk chunk)
        $ Effect $ mnext >>= pure . producer

    producer (EndStream ma) = Effect $ do
      a <- ma
      return $ Yield (ServerAgency TokBusy) MsgEndStream (Done TokDone a)
