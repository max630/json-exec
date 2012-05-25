{-# LANGUAGE NoMonomorphismRestriction #-}
module EnumeratorServer where

import Data.Attoparsec.Enumerator (iterParser)
import Data.Aeson.Parser (json)
import Data.Enumerator (run_)
import Data.Enumerator.Binary (enumHandle)
import GHC.IO.Handle.FD (stdin, stdout)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Data.Enumerator (($$), printChunks)

iter = iterParser json

-- run the results with run_

-- blindly echoing chinks
echo0 = ((enumHandle 10240 stdin) $$ (printChunks True))


