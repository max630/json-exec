{-# LANGUAGE OverloadedStrings, ViewPatterns #-}
module RPCTypes where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as M

import Data.Aeson ((.=), (.:))
import Data.Functor ((<$>))
import Control.Applicative ((<*>))

data Request params = Request !A.Value !String !params deriving (Eq, Show)

data Response result =  Response !A.Value !A.Value result deriving (Eq, Show)

data Notification = Notification !A.Value deriving (Eq, Show)

instance (A.ToJSON p) => A.ToJSON (Request p) where
  toJSON (Request id method params) = A.object ["id" .= id, "method" .= method, "params" .= params]

instance (A.FromJSON p) => A.FromJSON (Request p) where
  parseJSON (A.Object h) = Request <$> h .: "id" <*> h .: "method" <*> h .: "params"
  parseJSON v = fail ("parseJSON: Request must be object, got " ++ show v)

instance (A.ToJSON p) => A.ToJSON (Response p) where
  toJSON (Response id error result) = A.object ["id" .= id, "error" .= error, "result" .= result]

instance (A.FromJSON p) => A.FromJSON (Response p) where
  parseJSON (A.Object h) = Response <$> h .: "id" <*> h .: "error" <*> h .: "result"
  parseJSON v = fail ("parseJSON: Response must be object, got " ++ show v)

instance A.ToJSON Notification where
  toJSON (Notification (A.Object m)) = A.Object (M.insert "id" A.Null m)
  toJSON (Notification v) = A.object ["id" .= A.Null, "content" .= v]

instance A.FromJSON Notification where
  parseJSON o@(A.Object (M.lookup "id" -> Just A.Null)) = return (Notification o)
  parseJSON _ = fail "fromJSON: notification must be an object with null id"
