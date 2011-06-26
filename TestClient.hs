module Main where

import qualified RPC
import qualified System.Process as P
import qualified Data.ByteString as B
import System.IO.Unsafe (unsafePerformIO)
import IO (hSetBuffering, BufferMode(NoBuffering))

{-# NOINLINE connection #-}
connection =
  do
    (Just inH, Just outH, Nothing, process) <- P.createProcess $ P.CreateProcess {
                                            P.cmdspec = P.RawCommand "./TestServer" [],
                                            P.cwd = Nothing,
                                            P.env = Nothing,
                                            P.std_in = P.CreatePipe,
                                            P.std_out = P.CreatePipe,
                                            P.std_err = P.Inherit,
                                            P.close_fds = True}
    hSetBuffering outH NoBuffering
    hSetBuffering inH NoBuffering
    RPC.newConnection (B.hGetSome outH 1024) (B.hPut inH)

main = return ()
