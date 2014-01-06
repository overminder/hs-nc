module Main where

import Nc

import Control.Applicative
import Control.Concurrent
import System.Environment

main = do
  lock <- newEmptyMVar
  peer <- mkPeer =<< parseArg =<< getArgs
  let onDone = putMVar lock ()
  handlePeer (Left peer) onDone
  takeMVar lock >> return ()

