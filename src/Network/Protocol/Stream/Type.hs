{-# LANGUAGE DataKinds          #-}
{-# LANGUAGE EmptyCase          #-}
{-# LANGUAGE FlexibleInstances  #-}
{-# LANGUAGE GADTs              #-}
{-# LANGUAGE InstanceSigs       #-}
{-# LANGUAGE PolyKinds          #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies       #-}


module Network.Protocol.Stream.Type where

import           Data.Void (Void)

import           Network.TypedProtocol.Core


data Stream id chunk where
     StIdle :: Stream id chunk
     StBusy :: Stream id chunk
     StDone :: Stream id chunk

instance Protocol (Stream id chunk) where

    data Message (Stream id chunk) from to where
         -- | Request data with a given id
         --
         MsgGet :: Int -- size of a chunk
                -> id  -- resource id
                -> Message (Stream id chunk) 'StIdle 'StBusy

         -- | stream a single chunk
         --
         MsgChunk :: chunk
                  -> Message (Stream id chunk) 'StBusy 'StBusy

         -- | Streaming is done.
         --
         MsgEndStream :: Message (Stream id chunk) 'StBusy 'StDone

    -- | Singletons of states in which client has the agency.
    --
    data ClientHasAgency st where
         TokIdle :: ClientHasAgency 'StIdle

    -- | Singletons of states in which the server has the agency.
    --
    data ServerHasAgency st where
         TokBusy :: ServerHasAgency 'StBusy

    -- | Singletons of terminal states.
    --
    data NobodyHasAgency st where
         TokDone :: NobodyHasAgency 'StDone

    exclusionLemma_ClientAndServerHaveAgency :: forall (st :: Stream id chunk).
                                                ClientHasAgency st
                                             -> ServerHasAgency st
                                             -> Void
    exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}

    exclusionLemma_NobodyAndClientHaveAgency :: forall (st :: Stream id chunk).
                                                NobodyHasAgency st
                                             -> ClientHasAgency st
                                             -> Void
    exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}

    exclusionLemma_NobodyAndServerHaveAgency :: forall (st :: Stream id chunk).
                                                NobodyHasAgency st
                                             -> ServerHasAgency st
                                             -> Void
    exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}

deriving instance (Show id, Show chunk)
               => Show (Message (Stream id chunk) from to)

deriving instance (Eq id, Eq chunk)
               => Eq (Message (Stream id chunk) from to)
