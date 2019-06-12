{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Protocol.Stream.Example where


-- import qualified Data.Conduit as C
import qualified Pipes

import           Network.Protocol.Stream.Client
import           Network.Protocol.Stream.Server


clientMap :: forall m id chunk.
             Applicative m
          => id
          -> StreamClient m id chunk [chunk]
clientMap id = Request id (pure $ collect [])
  where
    collect :: [chunk] -> Collect m id chunk [chunk]
    collect chunks = Collect {
        handleChunk = \chunk -> pure (collect (chunk : chunks)),
        handleDone  = chunks
      }


streamServerFromPipe
    :: forall m id chunk i a.
       Monad m
    => (id -> Pipes.Producer chunk m a)
    -> StreamServer m id chunk a
streamServerFromPipe f = StreamServer $ streamProducerFromPipe . f
  where
    streamProducerFromPipe
        :: Pipes.Producer chunk m a
        -> m (Producer m chunk a)
    streamProducerFromPipe pr = do
      r <- Pipes.next pr

      case r of
        Left a ->
          return $ Result a 

        Right (chunk, pr') ->
          return $ Chunk chunk (streamProducerFromPipe pr')
