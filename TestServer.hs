module Main where

import qualified RPC
import qualified Data.ByteString as B
import IO (stdin, stdout)
import Control.Concurrent.MVar (newMVar, putMVar)

main =
  do
    var <- newMVar 0
    connection <- RPC.newConnection (B.hGetSome stdin 1024) (B.hPut stdout)
    RPC.registerMethodHandler connection "exp" ((\x -> return (exp x)) :: Double -> IO Double)
    putMVar var 10
