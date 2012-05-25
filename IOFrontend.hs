{-# LANGUAGE NoMonomorphismRestriction #-}
module IOFrontend where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.Aeson as A
import qualified Data.Aeson.Parser as AP
import qualified Data.Aeson.Encode as AE
import qualified Data.Attoparsec.ByteString as APB
import qualified Data.Attoparsec.Combinator as APC
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import qualified GHC.IO.Handle as GIO
import qualified GHC.IO.Handle.FD as GIOF

import Data.Attoparsec.Enumerator (iterParser)

mkHandler input output = (handle, send, close)
  where
    handle dispatch =
      E.run_ (EB.enumHandle 10240 input
            E.$$ (E.sequence (iterParser jsonOrEOF)
                E.=$ EL.isolateWhile isRight
                E.=$ EL.map (\(Right v) -> v)
                E.=$ EL.mapM_ dispatch))
    send value = MIO.liftIO $ BSL.hPut output $ AE.encode value
    close = GIO.hClose output

isRight (Left _) = False
isRight (Right _) = True
  
jsonOrEOF = APB.takeWhile (APB.inClass " \t\r\n") >> APC.eitherP (APB.try APB.endOfInput) AP.json

-- check mkHandler - it must work for this case
_ = mkHandler GIOF.stdin GIOF.stdout :: ((A.Value -> IO ()) -> IO (), A.Value -> IO (), IO ())
