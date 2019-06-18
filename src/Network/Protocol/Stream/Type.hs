{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE EmptyCase #-}


module Network.Protocol.Stream.Type where

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

    data ClientHasAgency st where
         TokIdle :: ClientHasAgency 'StIdle

    data ServerHasAgency st where
         TokBusy :: ServerHasAgency 'StBusy

    data NobodyHasAgency st where
         TokDone :: NobodyHasAgency 'StDone

    exclusionLemma_ClientAndServerHaveAgency TokIdle tok = case tok of {}
    exclusionLemma_NobodyAndClientHaveAgency TokDone tok = case tok of {}
    exclusionLemma_NobodyAndServerHaveAgency TokDone tok = case tok of {}

deriving instance (Show id, Show chunk)
               => Show (Message (Stream id chunk) from to)

deriving instance (Eq id, Eq chunk)
               => Eq (Message (Stream id chunk) from to)
