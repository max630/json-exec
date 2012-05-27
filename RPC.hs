{-# OPTIONS_GHC #-}
{-# LANGUAGE ExistentialQuantification, ViewPatterns, OverloadedStrings, NoMonomorphismRestriction, ScopedTypeVariables #-}
module RPC where

import qualified Data.Aeson as A
import qualified Data.HashTable as H
import qualified Data.HashMap.Strict as M
import qualified GHC.IO.Handle as GIO
import qualified Control.Concurrent.MVar as MV
import qualified Control.Exception as E
import qualified Data.Text as T
import qualified Data.Vector as V
import qualified System.Process as P

import Data.Aeson ((.=))
import Data.Attoparsec.Number (Number(I))
import Control.Monad (when)
import System.IO (hPutStrLn, stderr)
import Control.Concurrent (forkIO)
import Data.Hashable (Hashable(hash))

import IOFrontend (mkHandler)

data Connection a b = Connection { conn_send :: A.Value -> IO (),
                                  conn_pending :: a,
                                  conn_counter :: IO Integer,
                                  conn_methods :: b}

data Handler = forall a b . (A.FromJSON a, A.ToJSON b) => Handler (a -> IO b)


-- registerMethodHandler :: (FromJSON a, ToJSON b) => Connection -> String -> (a -> IO b) -> IO ()
registerMethodHandler conn name handler = H.insert (conn_methods conn) name (Handler handler)

mlookup ks kx = if all (flip M.member kx) ks then Just (map ((M.!) kx) ks) else Nothing

-- getMethod :: (ToJSON a, FromJSON b) => Connection -> String -> IO (a -> IO b)
getMethod conn name = return f
  where
    f a =
      do
        id <- (conn_counter conn)
        var <- MV.newEmptyMVar
        E.bracket_
          (H.insert (conn_pending conn) (A.Number $ I id) (MV.putMVar var))
          (H.delete (conn_pending conn) (A.Number $ I id))
          (do
            conn_send conn (A.object ["id" .= id, "method" .= A.String (T.pack name), "params" .= [A.toJSON a]])
            response <- MV.takeMVar var
            case response of
              A.Object (mlookup ["result", "error"] -> Just [result, A.Null]) ->
                case A.fromJSON result of
                  A.Success resultValue -> return resultValue
                  A.Error err -> fail ("Invalid return type: " ++ show err)
              A.Object (mlookup ["result", "error"] -> Just [A.Null, error]) ->
                fail ("Service failed: " ++ show error)
              _ -> fail ("Invalid response:" ++ show response))


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
              conn_methods = methods
              }
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

dispatch conn (A.Object (mlookup ["id", "method", "params"] -> Just [A.Null, method, params])) = undefined -- TODO: notifications
dispatch conn (A.Object (mlookup ["id", "method", "params"] -> Just [id, A.String name, A.Array (V.toList -> [params])])) =
  do
    handlerMb <- H.lookup (conn_methods conn) (T.unpack name)
    case handlerMb of
      Just (Handler handler) ->
        do
          response <-
            case A.fromJSON params of
              A.Success paramsV ->
                    E.catch
                      (do
                        res <- handler paramsV
                        return (A.object ["id" .= id, "error" .= A.Null, "result" .= A.toJSON res]))
                      (\err -> return (errorResponse $ show (err :: E.SomeException)))
              A.Error err -> return $ errorResponse err
          conn_send conn response
      Nothing -> conn_send conn (errorResponse ("Unknown method: " ++ T.unpack name))
  where
    errorResponse errorString = A.object ["id" .= id, "error" .= errorString, "result" .= A.Null]

dispatch conn o@(A.Object (mlookup ["id", "result", "error"] -> Just [id, _, _])) =
  do
    handler <- H.lookup (conn_pending conn) id
    case handler of
      Just h -> h o
      Nothing -> fail ("Unknown response:" ++ show o)
dispatch _ o = fail ("Unknown message:" ++ show o)

{-
_test1 r w = do
  c <- newConnection False r w
  registerMethodHandler c "m1" (undefined :: Int -> IO String)
  registerMethodHandler c "m2" (undefined :: String -> IO Int)
  m3 <- (getMethod c "m3" :: IO (Int -> IO String))
  m4 <- (getMethod c "m4" :: IO (String -> IO Int))
  return c
-}
