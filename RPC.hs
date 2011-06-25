{-# OPTIONS_GHC #-}
{-# LANGUAGE ExistentialQuantification, ViewPatterns, OverloadedStrings #-}
module RPC where

import Data.ByteString (ByteString, hPutStr)
import Data.Aeson (FromJSON, fromJSON, ToJSON, toJSON, Value(Array, Object, Number, Null), object, (.=), Result(Success, Error), json)
import Data.Attoparsec (parseWith)
import qualified Data.Attoparsec as AP
import Data.Attoparsec.Number (Number(I))
import Data.Aeson.Encode (fromValue)
import Blaze.ByteString.Builder (toByteString)
import Data.Vector (singleton)
import Data.HashTable (insert, delete)
import qualified Data.Map as M
import qualified Data.HashTable as H
import Control.Concurrent.MVar (newEmptyMVar, putMVar, takeMVar)
import Control.Exception (bracket_)

data Connection a b = Connection { conn_write :: ByteString -> IO (),
                                  conn_read :: IO ByteString,
                                  conn_pending :: a,
                                  conn_counter :: IO Integer,
                                  conn_handlers :: b}

data Handler = forall a b . (FromJSON a, ToJSON b) => Handler (a -> IO b)


-- registerMethodHandler :: (FromJSON a, ToJSON b) => Connection -> String -> (a -> IO b) -> IO ()
registerMethodHandler conn name handler = insert (conn_handlers conn) name (Handler handler)

mlookup ks kx = if all (flip M.member kx) ks then Just (map ((M.!) kx) ks) else Nothing

-- getMethod :: (ToJSON a, FromJSON b) => Connection -> String -> IO (a -> IO b)
getMethod conn name = return f
  where
    f a =
      do
        id <- (conn_counter conn)
        var <- newEmptyMVar
        bracket_
          (insert (conn_pending conn) name (putMVar var))
          (delete (conn_pending conn) name)
          (do 
            conn_write conn (toByteString $ fromValue $ object ["id" .= id, "method" .= name, "params" .= singleton (toJSON a)])
            response <- takeMVar var
            case fromJSON response of
              Success res -> return res
              Error err -> fail err)

      

handleInputs conn = loop ""
  where
    loop buffer =
      do
        result <- parseWith (conn_read conn) json buffer
        case result of
          AP.Fail _ _ msg -> fail ("failed to parse input: " ++ msg)
          AP.Partial _ -> fail "Partial must not be here"
          AP.Done newBuffer message ->
            do
              dispatch conn message
              loop newBuffer

dispatch conn (Object (mlookup ["id", "method", "params"] -> Just [Null, method, params])) = undefined -- TODO: notifications
dispatch conn (Object (mlookup ["id", "method", "params"] -> Just [id, method, params])) = undefined -- TODO: implement lookup in conn
dispatch conn o@(Object (mlookup ["id", "response", "error"] -> Just [id, _, _])) =
  do
    handler <- H.lookup (conn_pending conn) id
    case handler of
      Just h -> h o
      Nothing -> fail ("Unknown response:" ++ show o)
dispatch _ o = fail ("Unknown message:" ++ show o)
