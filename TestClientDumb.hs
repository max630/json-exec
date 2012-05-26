{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
module TestClient where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M
import qualified GHC.IO.Handle.FD as GIOF
import qualified System.Process as P
import qualified Control.Concurrent.MVar as MV

import Data.Aeson ((.=))
import Control.Concurrent (forkIO)

import IOFrontend (mkHandler)

main = do
  (Just toChild, Just fromChild, Nothing, process) <-
        P.createProcess (P.CreateProcess (P.RawCommand "./TestServer" [])
                                         Nothing
                                         Nothing
                                         P.CreatePipe
                                         P.CreatePipe
                                         P.Inherit
                                         True
                                         False)
  let (handle, send, close) = mkHandler fromChild toChild
  v <- MV.newEmptyMVar
  forkIO (handle (\v -> putStrLn ("Received: " ++ show v)) >> MV.putMVar v 0)
  send $ A.object ["id" .= (1 :: Int), "method" .= ("exp" :: String), "params" .= [A.toJSON (1 :: Int)]]
  MV.takeMVar v

