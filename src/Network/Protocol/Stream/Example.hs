{-# LANGUAGE GADTs               #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE TypeFamilies        #-}

module Network.Protocol.Stream.Example where

import qualified Pipes
import           Data.Foldable (traverse_)

import           Network.TypedProtocol.Core

import           Network.Protocol.Stream.Type
import           Network.Protocol.Stream.Client
import           Network.Protocol.Stream.Server


clientMap :: forall m id chunk.
             Applicative m
          => id
          -> StreamClient m id chunk [chunk]
clientMap id_ = Request id_ (pure $ collect [])
  where
    collect :: [chunk] -> Collect m id chunk [chunk]
    collect chunks = Collect {
        handleChunk = \chunk -> pure (collect (chunk : chunks)),
        handleDone  = chunks
      }


streamServerFromPipe
    :: forall m id chunk a.
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

type family MapStream id id' (k :: Stream id chunk) :: Stream id' chunk where
    MapStream id id' 'StIdle = 'StIdle
    MapStream id id' 'StBusy = 'StBusy
    MapStream id id' 'StDone = 'StDone

-- | Poor man's resource handling! (Here we don't guarantee that during the
-- resources will get closed properly in case of errors).  This is one of the
-- limitations of this framework, a proper way  would involve 'ResourceT' trick
-- (in the same way as normally one does with pipes or conduits).
--
withResource :: forall id handle chunk m a.
                Monad m
             => (id -> m handle) -- open resource
             -> (handle -> m ()) -- close resource
             -> Peer (Stream handle chunk) 'AsServer 'StIdle m a
             -> Peer (Stream id chunk) 'AsServer 'StIdle m a
withResource openR closeR = go Nothing
  where
    go :: Maybe handle
       -> Peer (Stream handle chunk) 'AsServer (st                     :: Stream handle chunk) m a
       -> Peer (Stream id     chunk) 'AsServer (MapStream handle id st :: Stream id     chunk) m a

    go Nothing (Await (ClientAgency TokIdle) f) =
      Await (ClientAgency TokIdle) $ \(MsgGet id_) -> Effect $ do
        handle <- openR id_
        return $ go (Just handle) $ f (MsgGet handle)

    go (Just _) (Await _ _) = error "impossible happend"

    go handle (Effect mf) = Effect $ go handle <$> mf

    go handle (Yield (ServerAgency TokBusy) (MsgChunk chunk) k) =
      Yield (ServerAgency TokBusy) (MsgChunk chunk) (go handle k)

    go handle (Yield (ServerAgency TokBusy) MsgDone k) =
      Yield (ServerAgency TokBusy) MsgDone (go handle k)

    go handle (Done TokDone a) = Effect $ do
      _ <- traverse_ closeR handle
      return $ Done TokDone a
