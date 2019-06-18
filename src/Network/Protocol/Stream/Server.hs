{-# LANGUAGE GADTs #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Protocol.Stream.Server where


import           Network.TypedProtocol.Core

import           Network.Protocol.Stream.Type


data StreamServer m id chunk a = StreamServer {
    handleStream :: id
                 -- ^ resouce id
                 -> Int
                 -- ^ chunk size
                 -> m (Producer m id chunk a)

  , handleDone   :: a
  }


data Producer m id chunk a where
     Chunk
       :: chunk
       -> m (Producer m id chunk a)
       -> Producer m id chunk a

     EndStream
       :: m (StreamServer m id chunk a)
       -> Producer m id chunk a


streamServerPeer
    :: forall m id chunk a.
       Monad m
    => StreamServer m id chunk a
    -> Peer (Stream id chunk) 'AsServer 'StIdle m a
streamServerPeer StreamServer {handleStream, handleDone} =
    Await (ClientAgency TokIdle) $ \msg -> case msg of

      MsgGet id_ chunkSize ->
        Effect $ handleStream chunkSize id_ >>= pure . producer

      MsgDone -> Done TokDone handleDone
  where
    producer :: Producer m id chunk a
             -> Peer (Stream id chunk) 'AsServer 'StBusy m a

    producer (Chunk chunk mnext) =
      Yield (ServerAgency TokBusy) (MsgChunk chunk)
        $ Effect $ producer <$> mnext

    producer (EndStream mnext) =
      Yield (ServerAgency TokBusy) MsgEndStream
        $ Effect
        $ streamServerPeer <$> mnext
