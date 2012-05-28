{-# LANGUAGE OverloadedStrings #-}
module RPCTypes where

import qualified Data.Aeson as A

import Data.Aeson ((.=), (.:))
import Data.Functor ((<$>))
import Control.Applicative ((<*>))

data Request params = Request !A.Value !String !params deriving (Eq, Show)

data Response result =  Response !A.Value !A.Value result deriving (Eq, Show)

data Notification = Notification !A.Value

instance (A.ToJSON p) => A.ToJSON (Request p) where
  toJSON (Request id method params) = A.object ["id" .= id, "method" .= method, "params" .= params]

instance (A.FromJSON p) => A.FromJSON (Request p) where
  parseJSON (A.Object h) = Request <$> h .: "id" <*> h .: "method" <*> h .: "params"
  parseJSON v = fail ("parseJSON: Request must be object, got " ++ show v)
