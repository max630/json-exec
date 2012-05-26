module IOFrontend where

import qualified Control.Monad.IO.Class as MIO
import qualified Data.Aeson as A
import qualified Data.Attoparsec as AP
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Enumerator as E
import qualified Data.Enumerator.Binary as EB
import qualified Data.Enumerator.List as EL
import qualified GHC.IO.Handle as GIO
import qualified GHC.IO.Handle.FD as GIOF

import Data.Attoparsec.Enumerator (iterParser)

mkHandler :: MIO.MonadIO m => GIO.Handle -> GIO.Handle -> ((A.Value -> m ()) -> m (), A.Value -> m (), m ())
mkHandler input output = (handle, send, close)
  where
    handle dispatch =
      E.run_ (EB.enumHandle 10240 input
            E.$$ (E.sequence (iterParser jsonOrEOF)
                E.=$ EL.isolateWhile isRight
                E.=$ EL.map (\(Right v) -> v)
                E.=$ EL.mapM_ dispatch))
    send value = MIO.liftIO $ BSL.hPut output $ A.encode value
    close = MIO.liftIO $ GIO.hClose output

isRight (Left _) = False
isRight (Right _) = True
  
jsonOrEOF = AP.takeWhile (AP.inClass " \t\r\n") >> AP.eitherP (AP.try AP.endOfInput) A.json
