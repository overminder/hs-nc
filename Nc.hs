{-# LANGUAGE ScopedTypeVariables #-}
module Nc (
  parseArg,
  mkPeer,
  handlePeer,
  Action(..)
) where

import Control.Applicative
import Control.Exception
import Control.Monad
import Control.Concurrent
import qualified Data.ByteString as B
import Network.BSD
import Network.Socket
import System.IO
import System.Timeout

data Action
  = Listen SockAddr
  | Connect SockAddr

parseArg :: [String] -> IO Action
parseArg ["-l", portStr] = return $ Listen (SockAddrInet port host)
  where
    host = fromIntegral 16777343 -- 127.0.0.1
    port = fromIntegral (read portStr)

parseArg [hostStr, portStr] = do
  mbHost <- timeout (secToMs 3) getHost
  case mbHost of
    Nothing -> error $ "parseArg: hostname resolve timeout"
    Just host -> return $ Connect (SockAddrInet port host)
  where
    port = fromIntegral (read portStr)
    getHost = head . hostAddresses <$> getHostByName hostStr

mkPeer :: Action -> IO Socket
mkPeer (Listen addr) = do
  s <- mkReusableSock
  bind s addr
  listen s 5
  (peer, peerHostPort) <- accept s
  putStrLn $ "mkPeer: accepted connect from " ++ show peerHostPort
  return peer

mkPeer (Connect addr) = do
  s <- mkReusableSock
  mbOk <- timeout (secToMs 3) $ connect s addr
  case mbOk of
    Nothing -> error $ "mkPeer: connection timeout"
    Just _ -> do 
      putStrLn "mkPeer: connected"
      return s

mkReusableSock :: IO Socket
mkReusableSock = do
  s <- socket AF_INET Stream 0
  setSocketOption s ReuseAddr 1
  return s

handlePeer :: Either Socket Handle -> IO () -> IO ()
handlePeer eih onDone = do
  sockFile <- mkHandle
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering

  let
    sockToStdout = do
      recvData <- B.hGetSome sockFile 1024
      if B.null recvData
        then throwIO $ userError "remote host closed"
        else pass
      B.hPut stdout recvData
      hFlush stdout

    stdinToSock = do
      sendData <- B.hGetSome stdin 1024
      if B.null sendData
        then throwIO $ userError "EOF in stdin"
        else pass
      B.hPut sockFile sendData
      hFlush sockFile

  handleEx <- mkIdemptotent $ \(e :: SomeException) -> do
    putStrLn $ "handleEx: " ++ show e
    hClose sockFile
    onDone

  forkIO_ $ (`catch` handleEx) $ forever sockToStdout
  forkIO_ $ (`catch` handleEx) $ forever stdinToSock
 where
  mkHandle = case eih of
    Left s -> socketToHandle s ReadWriteMode
    Right h -> return h

forkIO_ m = forkIO m >> return ()

pass :: IO ()
pass = return ()

err :: String -> IO ()
err = throwIO . userError

mkIdemptotent :: (a -> IO ()) -> IO (a -> IO ())
mkIdemptotent m = do
  lock <- newMVar m
  return $ \arg -> do
    m <- swapMVar lock (\_ -> return ())
    m arg

secToMs = (* (1000 * 1000))
