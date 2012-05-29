{-# LANGUAGE ScopedTypeVariables #-}
module TestServer where

import qualified RPC
import System.IO (stdin, stdout)
import Control.Concurrent.MVar (newMVar, putMVar)

main =
  do
    var <- newMVar 0
    connection <- RPC.newConnectionHandles False stdin stdout
    let f (x :: Double) = if x >= 0 then return (exp x) else fail "Not supported for negatives"
    RPC.registerMethodHandler connection "exp" f
    putMVar var 10 -- this blocks the main thread indefinitely, allowing to handle requests
