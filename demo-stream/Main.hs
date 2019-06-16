{-# LANGUAGE DataKinds #-}

module Main where

import           Control.Exception (bracket)
import           Control.Monad (forever)
import qualified Pipes.ByteString
import qualified Data.ByteString as BS
import           System.IO
import           System.Environment (getArgs)

import           Control.Tracer (Tracer (..))
import           Network.TypedProtocol.Core
import           Network.TypedProtocol.Driver

import           Ouroboros.Network.Channel

import           Network.Protocol.Stream.Type
import           Network.Protocol.Stream.Codec
import           Network.Protocol.Stream.Client
import           Network.Protocol.Stream.Server
import           Network.Protocol.Stream.Example

import qualified Network.Socket as Socket

mkLocalSocketAddrInfo :: FilePath -> Socket.AddrInfo
mkLocalSocketAddrInfo socketPath =
    Socket.AddrInfo
      []
      Socket.AF_UNIX
      Socket.Stream
      Socket.defaultProtocol
      (Socket.SockAddrUnix socketPath)
      Nothing

defaultLocalSocketAddrPath :: FilePath
defaultLocalSocketAddrPath =  "./demo-stream.sock"

defaultLocalSocketAddrInfo :: Socket.AddrInfo
defaultLocalSocketAddrInfo = 
    mkLocalSocketAddrInfo defaultLocalSocketAddrPath

main :: IO ()
main = do
    args <- getArgs
    case args of
      "client":file:_ -> streamFileClient file
      "server":_      -> streamFileServer
      _               -> putStrLn $ mconcat $
                          [ "demo-stream client {file}\n"
                          , "demo-stream server\n"
                          ]

streamFileClient :: FilePath -> IO ()
streamFileClient path =
    bracket
      (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol)
      Socket.close
      $ \fd -> do
        Socket.connect fd (Socket.addrAddress defaultLocalSocketAddrInfo)
        _ <- runPeer
          (Tracer $ putStrLn . show)
          codecStream
          (socketAsChannel fd)
          peer
        return ()
  where
    peer :: Peer (Stream FilePath BS.ByteString) 'AsClient 'StIdle IO [BS.ByteString]
    peer = streamClientPeer (clientMap path)

streamFileServer :: IO ()
streamFileServer =
    bracket
      (Socket.socket Socket.AF_UNIX Socket.Stream Socket.defaultProtocol)
      Socket.close
      $ \fd -> do
        --
        -- simple accept loop
        --
        Socket.bind fd (Socket.addrAddress defaultLocalSocketAddrInfo)
        Socket.listen fd 1
        forever $ do
          (fd', _) <- Socket.accept fd
          runPeer
            (Tracer $ putStrLn . show)
            codecStream
            (socketAsChannel fd')
            peer

  where
    peer :: Peer (Stream FilePath BS.ByteString) 'AsServer 'StIdle IO ()
    peer = withResource (\fp -> openFile fp ReadMode) hClose
          $ streamServerPeer
          $ streamServerFromPipe Pipes.ByteString.fromHandle
