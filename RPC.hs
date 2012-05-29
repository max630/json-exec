{-# OPTIONS_GHC #-}
{-# LANGUAGE ExistentialQuantification, ViewPatterns, OverloadedStrings, NoMonomorphismRestriction, ScopedTypeVariables #-}
module RPC where

import qualified Data.Aeson as A
import qualified Data.HashTable as H
import qualified Control.Concurrent.MVar as MV
import qualified Control.Exception as E
import qualified System.Process as P

import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import Control.Concurrent (forkIO)
import Data.Hashable (Hashable(hash))

import IOFrontend (mkHandler)
import RPCTypes (Response(Response), Request(Request), Notification(Notification), parseM)

import Control.Applicative (Alternative((<|>)))
import Control.Monad (guard)

data Connection a b = Connection { conn_send :: A.Value -> IO (),
                                  conn_pending :: a,
                                  conn_counter :: IO Integer,
                                  conn_methods :: b}

data Handler = forall a b . (A.FromJSON a, A.ToJSON b) => Handler (a -> IO b)


-- registerMethodHandler :: (FromJSON a, ToJSON b) => Connection -> String -> (a -> IO b) -> IO ()
registerMethodHandler conn name handler = H.insert (conn_methods conn) name (Handler handler)

-- getMethod :: (ToJSON a, FromJSON b) => Connection -> String -> IO (a -> IO b)
getMethod conn name = return f
  where
    f a =
      do
        idNum <- (conn_counter conn)
        let id = A.toJSON idNum
        var <- MV.newEmptyMVar
        E.bracket_
          (H.insert (conn_pending conn) id (MV.putMVar var))
          (H.delete (conn_pending conn) id)
          (do
            conn_send conn (A.toJSON (Request (A.toJSON id) name [a]))
            response <- MV.takeMVar var
            -- TODO: make error reporting more thorough
            -- current view is for tutoring only
            parseM ((do
                Response _ A.Null result <- A.parseJSON response
                return (return result))
              <|> (do
                Response _ error A.Null <- A.parseJSON response
                guard (error /= A.Null)
                return (fail ("Service failed: " ++ show error))
              <|> (fail ("Cannot recognize response: " ++ show response)))))


newConnectionHandles debug input output =
  do
    let (handle, send, close) = mkHandler input output
    pending <- H.new (==) (fromInteger . toInteger . hash)
    methods <- H.new (==) (fromInteger . toInteger . hash)
    writeVar <- MV.newEmptyMVar
    counter <- MV.newMVar 0
    let
      writer =
        do
          value <- MV.takeMVar writeVar
          when debug $ hPutStrLn stderr ("Writing: " ++ show value)
          send value
          when debug $ hPutStrLn stderr ("Written")
          writer
      reader =
        do
          handle (\v -> do
                    when debug $ hPutStrLn stderr ("Read: " ++ show v)
                    dispatch conn v)
      conn = Connection {
              conn_send = (MV.putMVar writeVar),
              conn_pending = pending,
              conn_counter = MV.modifyMVar counter (\v -> return (v + 1, v)),
              conn_methods = methods }
    forkIO reader
    forkIO writer
    return conn

newConnectionCommand debug cmdSpec =
  do
    (Just inH, Just outH, Nothing, process) <- P.createProcess $ P.CreateProcess {
                                            P.cmdspec = cmdSpec,
                                            P.cwd = Nothing,
                                            P.env = Nothing,
                                            P.std_in = P.CreatePipe,
                                            P.std_out = P.CreatePipe,
                                            P.std_err = P.Inherit,
                                            P.close_fds = True,
                                            P.create_group = False}
    newConnectionHandles debug outH inH

dispatch conn value =
    parseM ((do
              Notification val <- A.parseJSON value
              (fail "notifications not supported"))
            <|> (do
              Request id name [paramValue] <- A.parseJSON value
              return (do
                handlerMb <- H.lookup (conn_methods conn) name
                case handlerMb of
                  Just (Handler handler) -> E.catch (handle id paramValue handler) (\err -> sendError id (show (err :: E.SomeException)))
                  Nothing -> sendError id ("Method \"" ++ name ++ "\" not found")))
            <|> (do
              Response id _ (_ :: A.Value) <- A.parseJSON value
              return (do
                rhMb <- H.lookup (conn_pending conn) id
                case rhMb of
                  Just rh -> rh value
                  Nothing -> fail ("Unknown request id:" ++ show id))))
  where
    sendError id err = conn_send conn (A.toJSON (Response id (A.toJSON err) A.Null))
    handle id paramValue handler =
      parseM (do
          param <- A.parseJSON paramValue
          return (do
            result <- handler param
            conn_send conn (A.toJSON (Response id A.Null result))))

{-
_test1 r w = do
  c <- newConnection False r w
  registerMethodHandler c "m1" (undefined :: Int -> IO String)
  registerMethodHandler c "m2" (undefined :: String -> IO Int)
  m3 <- (getMethod c "m3" :: IO (Int -> IO String))
  m4 <- (getMethod c "m4" :: IO (String -> IO Int))
  return c
-}
