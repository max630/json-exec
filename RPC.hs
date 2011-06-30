{-# OPTIONS_GHC #-}
{-# LANGUAGE ExistentialQuantification, ViewPatterns, OverloadedStrings, NoMonomorphismRestriction #-}
module RPC where

import Data.ByteString (ByteString, hPutStr)
import qualified Data.ByteString as B
import Data.Aeson (FromJSON, fromJSON, ToJSON, toJSON, Value(Array, Object, Number, Null, String), object, (.=), Result(Success, Error), json)
import Data.Attoparsec (parseWith)
import qualified Data.Attoparsec as AP
import Data.Attoparsec.Number (Number(I))
import Data.Aeson.Encode (fromValue)
import qualified Data.Text as T
import Blaze.ByteString.Builder (toByteString)
import Data.Vector (singleton)
import Data.HashTable (insert, delete)
import qualified Data.Map as M
import qualified Data.HashTable as H
import Control.Concurrent.MVar (newEmptyMVar, newMVar, putMVar, takeMVar, modifyMVar)
import Control.Exception (bracket_)
import Control.Concurrent (forkIO)
import Data.Hashable (hash)
import IO (hPutStrLn, hSetBuffering, BufferMode(NoBuffering), stderr)
import Data.Vector (toList)
import qualified System.Process as P
import Monad (when)

data Connection a b = Connection { conn_send :: Value -> IO (),
                                  conn_pending :: a,
                                  conn_counter :: IO Integer,
                                  conn_methods :: b}

data Handler = forall a b . (FromJSON a, ToJSON b) => Handler (a -> IO b)


-- registerMethodHandler :: (FromJSON a, ToJSON b) => Connection -> String -> (a -> IO b) -> IO ()
registerMethodHandler conn name handler = insert (conn_methods conn) name (Handler handler)

mlookup ks kx = if all (flip M.member kx) ks then Just (map ((M.!) kx) ks) else Nothing

-- getMethod :: (ToJSON a, FromJSON b) => Connection -> String -> IO (a -> IO b)
getMethod conn name = return f
  where
    f a =
      do
        id <- (conn_counter conn)
        var <- newEmptyMVar
        bracket_
          (insert (conn_pending conn) (Number $ I id) (putMVar var))
          (delete (conn_pending conn) (Number $ I id))
          (do
            conn_send conn (object ["id" .= id, "method" .= String (T.pack name), "params" .= singleton (toJSON a)])
            response <- takeMVar var
            case response of
              Object (mlookup ["result", "error"] -> Just [result, Null]) ->
                case fromJSON result of
                  Success resultValue -> return resultValue
                  Error err -> fail ("Invalid return type: " ++ show err)
              Object (mlookup ["result", "error"] -> Just [Null, error]) ->
                fail ("Service failed: " ++ show error)
              _ -> fail ("Invalid response:" ++ show response))


newConnection debug readF writeF =
  do
    pending <- H.new (==) (fromInteger . toInteger . hash)
    methods <- H.new (==) (fromInteger . toInteger . hash)
    writeVar <- newEmptyMVar
    counter <- newMVar 0
    let
      writer =
        do
          value <- takeMVar writeVar
          when debug $ hPutStrLn stderr ("Writing: " ++ show value)
          writeF (toByteString $ fromValue value)
          when debug $ hPutStrLn stderr ("Written")
          writer
      reader buffer =
        do
          when debug $ hPutStrLn stderr ("Reading")
          parseResult <- parseWith readF json buffer
          when debug $ hPutStrLn stderr ("Read: " ++ show parseResult)
          case parseResult of
            AP.Fail _ _ msg -> fail ("failed to parse input: " ++ msg)
            AP.Partial _ -> fail "Partial must not be here"
            AP.Done newBuffer message ->
              do
                dispatch conn message
                reader newBuffer
      conn = Connection {
              conn_send = (putMVar writeVar),
              conn_pending = pending,
              conn_counter = modifyMVar counter (\v -> return (v + 1, v)),
              conn_methods = methods
              }
    forkIO (reader "")
    forkIO writer
    return conn

newConnectionHandles debug handleIn handleOut =
  do
    hSetBuffering handleIn NoBuffering
    hSetBuffering handleOut NoBuffering
    newConnection debug (B.hGetSome handleIn 1024) (B.hPut handleOut)

newConnectionCommand debug cmdSpec =
  do
    (Just inH, Just outH, Nothing, process) <- P.createProcess $ P.CreateProcess {
                                            P.cmdspec = cmdSpec,
                                            P.cwd = Nothing,
                                            P.env = Nothing,
                                            P.std_in = P.CreatePipe,
                                            P.std_out = P.CreatePipe,
                                            P.std_err = P.Inherit,
                                            P.close_fds = True}
    newConnectionHandles debug outH inH

dispatch conn (Object (mlookup ["id", "method", "params"] -> Just [Null, method, params])) = undefined -- TODO: notifications
dispatch conn (Object (mlookup ["id", "method", "params"] -> Just [id, String name, Array (toList -> [params])])) =
  do
    handlerMb <- H.lookup (conn_methods conn) (T.unpack name)
    case handlerMb of
      Just (Handler handler) ->
        do
          response <-
            case fromJSON params of
              Success paramsV ->
                    catch
                      (do
                        res <- handler paramsV
                        return (object ["id" .= id, "error" .= Null, "result" .= toJSON res]))
                      (\err -> return (errorResponse $ show err))
              Error err -> return $ errorResponse err
          conn_send conn response
      Nothing -> conn_send conn (errorResponse ("Unknown method: " ++ T.unpack name))
  where
    errorResponse errorString = object ["id" .= id, "error" .= errorString, "result" .= Null]

dispatch conn o@(Object (mlookup ["id", "result", "error"] -> Just [id, _, _])) =
  do
    handler <- H.lookup (conn_pending conn) id
    case handler of
      Just h -> h o
      Nothing -> fail ("Unknown response:" ++ show o)
dispatch _ o = fail ("Unknown message:" ++ show o)

_test1 r w = do
  c <- newConnection False r w
  registerMethodHandler c "m1" (undefined :: Int -> IO String)
  registerMethodHandler c "m2" (undefined :: String -> IO Int)
  m3 <- (getMethod c "m3" :: IO (Int -> IO String))
  m4 <- (getMethod c "m4" :: IO (String -> IO Int))
  return c
