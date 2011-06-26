module Main where

import qualified RPC
import qualified System.Process as P

makeConnection =
  do
    (Just inH, Just outH, Nothing, process) <- P.createProcess $ P.CreateProcess {
                                            P.cmdspec = P.RawCommand "./TestServer" [],
                                            P.cwd = Nothing,
                                            P.env = Nothing,
                                            P.std_in = P.CreatePipe,
                                            P.std_out = P.CreatePipe,
                                            P.std_err = P.Inherit,
                                            P.close_fds = True}
    RPC.newConnectionHandles outH inH

main = return ()
