{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeFamilies        #-}

module Network.Protocol.Stream.Example where

import qualified Pipes
import           Data.Foldable (traverse_)

import           Network.Protocol.Stream.Client
import           Network.Protocol.Stream.Server


clientMap :: forall m id chunk.
             Applicative m
          => [(id, Int)]
          -> StreamClient m id chunk [[chunk]]
clientMap = go []
  where
    go :: [[chunk]] -> [(id, Int)] -> StreamClient m id chunk [[chunk]]
    go chunks [] = ClientDone chunks
    go chunks ((id_, chunkSize) : reqs) =
      Request id_ chunkSize (pure $ collect chunks reqs)

    collect :: [[chunk]] -> [(id, Int)] -> Collect m id chunk [[chunk]]
    collect chunks reqs = Collect {
        handleChunk = \chunk -> pure $ collect (stack chunk chunks) reqs
      , handleEndStream = pure $ go chunks reqs
      }

    stack :: a -> [[a]] -> [[a]]
    stack a [] = [[a]]
    stack a (as : rest) = (a : as) : rest


streamServerFromPipe
    :: forall m id chunk a.
       Monad m
    => (Int -> id -> Pipes.Producer chunk m a)
    -> StreamServer m id chunk [a]
streamServerFromPipe f = go []
  where
    go :: [a] -> StreamServer m id chunk [a]
    go as = StreamServer {
        handleStream = \id_ chunkSize -> streamProducerFromPipe as (f chunkSize id_),
        handleDone   = as
      }

    streamProducerFromPipe
        :: [a]
        -> Pipes.Producer chunk m a
        -> m (Producer m id chunk [a])
    streamProducerFromPipe as pr = do
      r <- Pipes.next pr

      case r of
        Left a ->
          return $ EndStream $ return $ go (a:as)

        Right (chunk, pr') ->
          return $ Chunk chunk (streamProducerFromPipe as pr')


withResource
    ::  forall id handle chunk m a. Monad m
    => (id -> m handle)
    -> (handle -> m ())
    -> StreamServer m handle chunk a
    -> StreamServer m id chunk a
withResource openR closeR StreamServer {handleStream, handleDone} =
    StreamServer {
        handleStream = \id_ chunkSize -> do
          handle <- openR id_
          go Nothing <$> handleStream handle chunkSize
      , handleDone = handleDone
      }
  where
    go :: Maybe handle -> Producer m handle chunk a -> Producer m id chunk a
    go hndl (Chunk chunk mnext) = Chunk chunk (go hndl <$> mnext)
    go hndl (EndStream mnext) = EndStream $ do
      traverse_ closeR hndl
      withResource openR closeR <$> mnext
