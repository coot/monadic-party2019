{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE NamedFieldPuns      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Network.Protocol.Stream.Codec where

import           Codec.Serialise (Serialise)
import qualified Codec.Serialise as Serialise
import qualified Codec.CBOR.Encoding as CBOR
import qualified Codec.CBOR.Read     as CBOR
import qualified Codec.CBOR.Decoding as CBOR
import           Codec.CBOR.Encoding (encodeListLen, encodeWord)
import           Codec.CBOR.Decoding (decodeListLen, decodeWord)

import           Data.ByteString.Lazy (ByteString)

import           Ouroboros.Network.Codec

import           Network.Protocol.Stream.Type


codecStream :: forall id chunk.
               ( Serialise id
               , Serialise chunk
               )
            => Codec (Stream id chunk) 
                  CBOR.DeserialiseFailure
                  IO ByteString
codecStream = mkCodecCborLazyBS encodeMsg decodeMsg
  where
    encodeMsg :: forall (pr :: PeerRole) (st :: Stream id chunk) (st' :: Stream id chunk).
              PeerHasAgency pr st
           -> Message (Stream id chunk) st st'
           -> CBOR.Encoding

    encodeMsg (ClientAgency TokIdle) (MsgGet chunkSize id_) =
         encodeListLen 2 
      <> encodeWord 0
      <> encodeWord (fromIntegral chunkSize)
      <> Serialise.encode id_

    encodeMsg (ServerAgency TokBusy) (MsgChunk chunk) =
         encodeListLen 2
      <> encodeWord 1
      <> Serialise.encode chunk

    encodeMsg (ServerAgency TokBusy) MsgDone =
         encodeListLen 1
      <> encodeWord 2


    decodeMsg :: forall (pr :: PeerRole) (st :: Stream id chunk) s.
              PeerHasAgency pr st
           -> CBOR.Decoder s (SomeMessage st)
    decodeMsg tok = do
      len <- decodeListLen
      tag <- decodeWord
      case (tok, tag, len) of

        (ClientAgency TokIdle, 0, 2) -> do
          chunkSize <- decodeWord
          id_ <- Serialise.decode
          return $ SomeMessage (MsgGet (fromIntegral chunkSize) id_)

        (ServerAgency TokBusy, 1, 2) -> do
          chunk <- Serialise.decode
          return $ SomeMessage (MsgChunk chunk)

        (ServerAgency TokBusy, 2, 1) -> return $ SomeMessage MsgDone

        (_, _, _) -> fail $ "codecStream: unknown message: " ++ show tag

