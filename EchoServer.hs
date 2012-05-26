{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module EchoServer where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M
import qualified GHC.IO.Handle.FD as GIOF

import IOFrontend (mkHandler)

main = do
  let (handle, send, close) = mkHandler GIOF.stdin GIOF.stdout
  handle (\v -> send (A.Object (M.fromList [("id", A.Null), ("echo", v)])))
  close

