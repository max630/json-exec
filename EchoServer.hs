{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module EchoServer where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M
import qualified GHC.IO.Handle.FD as GIOF

import IOFrontend (mkHandler)

import System.IO (hPutStrLn, stderr)

main = do
  let (handle, send, close) = mkHandler GIOF.stdin GIOF.stdout
  hPutStrLn stderr "start handling"
  handle (\v -> do
            hPutStrLn stderr "received"
            send (A.Object (M.fromList [("id", A.Null), ("echo", v)])))
  close

