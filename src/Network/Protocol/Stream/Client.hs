{-# LANGUAGE DataKinds      #-}
{-# LANGUAGE GADTs          #-}
{-# LANGUAGE NamedFieldPuns #-}

module Network.Protocol.Stream.Client where


import           Network.TypedProtocol.Core

import           Network.Protocol.Stream.Type


-- | Stream client which requests data and collects responses.
--
data StreamClient m id chunk a where
     -- request data with id and provide callback for handling each chunk
     Request :: id
             -- ^ resource id
             -> Int
             -- ^ chunk size
             -> m (Collect m id chunk a)
             -> StreamClient m id chunk a

     ClientDone
             :: a
             -> StreamClient m id chunk a

instance Functor m => Functor (StreamClient m id chunk) where
    fmap f (Request id_ chunkSize mcollect) =
      Request id_ chunkSize ((fmap . fmap) f mcollect)
    fmap f (ClientDone a) = ClientDone (f a)


data Collect m id chunk a = Collect {
      handleChunk     :: chunk -> m (Collect m id chunk a),
      handleEndStream :: m (StreamClient m id chunk a)
    }


instance Functor m => Functor (Collect m id chunk) where
    fmap f Collect{handleChunk, handleEndStream} = Collect {
        handleChunk     = (fmap . fmap) f . handleChunk,
        handleEndStream = fmap f <$> handleEndStream
      }


streamClientPeer
  :: Monad m
  => StreamClient m id chunk a
  -> Peer (Stream id chunk) 'AsClient 'StIdle m a

streamClientPeer (Request id_ chunkSize mcollect) =
    Yield (ClientAgency TokIdle) (MsgGet chunkSize id_)
      $ Effect $ mcollect >>= pure . collectClientPeer

streamClientPeer (ClientDone a) =
    Yield (ClientAgency TokIdle) MsgDone (Done TokDone a)


collectClientPeer
  :: Monad m
  => Collect m id chunk a
  -> Peer (Stream id chunk) 'AsClient 'StBusy m a
collectClientPeer Collect {handleChunk, handleEndStream} =
    Await (ServerAgency TokBusy) $ \msg -> case msg of
      MsgChunk chunk -> Effect $ handleChunk chunk >>= pure . collectClientPeer
      MsgEndStream   -> Effect $ streamClientPeer <$> handleEndStream
