{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeFamilies        #-}

module Network.Protocol.Stream.Example where

import qualified Pipes
import           Data.Foldable (traverse_)

import           Network.Protocol.Stream.Client
import           Network.Protocol.Stream.Server


clientMap :: forall m id chunk.
             Applicative m
          => id
          -> Int
          -> StreamClient m id chunk [chunk]
clientMap id_ chunkSize = Request id_ chunkSize (pure $ collect [])
  where
    collect :: [chunk] -> Collect m id chunk [chunk]
    collect chunks = Collect {
        handleChunk = \chunk -> pure (collect (chunk : chunks)),
        handleDone  = chunks
      }


streamServerFromPipe
    :: forall m id chunk a.
       Monad m
    => (Int -> id -> Pipes.Producer chunk m a)
    -> StreamServer m id chunk a
streamServerFromPipe f = StreamServer $ \id_ chunkSize -> streamProducerFromPipe (f chunkSize id_)
  where
    streamProducerFromPipe
        :: Pipes.Producer chunk m a
        -> m (Producer m chunk a)
    streamProducerFromPipe pr = do
      r <- Pipes.next pr

      case r of
        Left a ->
          return $ Result (return a)

        Right (chunk, pr') ->
          return $ Chunk chunk (streamProducerFromPipe pr')


withResource
    ::  forall id handle chunk m a. Monad m
    => (id -> m handle)
    -> (handle -> m ())
    -> StreamServer m handle chunk a
    -> StreamServer m id chunk a
withResource openR closeR (StreamServer run) = StreamServer $ \id_ chunkSize -> do
    handle <- openR id_
    go Nothing <$> run handle chunkSize
  where
    go :: Maybe handle -> Producer m chunk a -> Producer m chunk a
    go hndl (Chunk chunk mnext) = Chunk chunk (go hndl <$> mnext)
    go hndl (Result ma) = Result $ traverse_ closeR hndl >> ma
