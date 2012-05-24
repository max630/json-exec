module Main where

import qualified RPC
import System.IO (stdin, stdout)
import Control.Concurrent.MVar (newMVar, putMVar)

main =
  do
    var <- newMVar 0
    connection <- RPC.newConnectionHandles True stdin stdout
    RPC.registerMethodHandler connection "exp" ((\x -> return (exp x)) :: Double -> IO Double)
    putMVar var 10 -- this blocks the main thread indefinitely, allowing to handle requests
