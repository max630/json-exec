{-# LANGUAGE NoMonomorphismRestriction #-}
module EnumeratorServer where

import Data.Attoparsec.Enumerator (iterParser)
import Data.Aeson.Parser (json)
import GHC.IO.Handle.FD (stdin, stdout)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Enumerator as E
import Data.Enumerator (($$), (=$), printChunks, run)
import Data.Enumerator.Binary (enumHandle)
import Data.Attoparsec.Combinator (eitherP)
import qualified Data.Attoparsec.ByteString as APB
import Data.Attoparsec.ByteString (endOfInput, inClass, try)

iter = iterParser json

jsonOrEOF = APB.takeWhile (inClass " \n") >> eitherP (try endOfInput) json

-- this does not work - requires json always. Looks like jsonOrEOF is more right way
jsonWoWhites = APB.takeWhile (inClass " \n") >> json

-- run the results with run_
main = run echo1
run1 = run

stdinEnum = enumHandle 10240 stdin

-- blindly echoing chinks
echo0 = (stdinEnum $$ printChunks True)

-- with parse
echo1 = (stdinEnum $$ (E.sequence (iterParser jsonOrEOF) =$ printChunks True))

echo2 = (stdinEnum $$ (E.sequence (iterParser jsonWoWhites) =$ printChunks True))
