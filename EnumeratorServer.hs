{-# LANGUAGE NoMonomorphismRestriction #-}
module EnumeratorServer where

import Data.Attoparsec.Enumerator (iterParser)
import Data.Aeson.Parser (json)
import GHC.IO.Handle.FD (stdin, stdout)
import Control.Monad.IO.Class (MonadIO(liftIO))
import qualified Data.Enumerator as E
import Data.Enumerator (($$), (=$), printChunks, run)
import qualified Data.Enumerator.Binary as EB
import Data.Attoparsec.Combinator (eitherP)
import qualified Data.Attoparsec.ByteString as APB
import Data.Attoparsec.ByteString (endOfInput, inClass, try)
import qualified Data.Enumerator.List as EL
import qualified Data.Aeson.Encode as AE
import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS

iter = iterParser json

jsonOrEOF = APB.takeWhile (inClass " \t\r\n") >> eitherP (try endOfInput) json

-- this does not work - requires json always. Looks like jsonOrEOF is more right way
jsonWoWhites = APB.takeWhile (inClass " \n") >> json

-- run the results with run_
main = run echo1
run1 = run

stdinEnum = EB.enumHandle 10240 stdin

-- blindly echoing chinks
echo_1 = (stdinEnum $$ printChunks True)

-- with parse and a problem
echo0 = (stdinEnum $$ (E.sequence (iterParser json) =$ printChunks True))

-- with parse
echo1 = (stdinEnum $$ (E.sequence (iterParser jsonOrEOF) =$ printChunks True))

echo2 = (stdinEnum $$ (E.sequence (iterParser jsonWoWhites) =$ printChunks True))

echo3 = (stdinEnum $$ (E.sequence (iterParser jsonOrEOF)) =$ EL.isolateWhile isRight =$ printChunks True)

-- this looks like the most correct way
-- or echo3
echo4 = (stdinEnum $$ (E.sequence (iterParser jsonOrEOF) =$ EL.mapM_ (\v -> putStrLn ("chunk: " ++ show v))))

-- this how it is going to be.
-- chaining sequence, isolateWhile and map is ugly
-- this is because json wants a value, so always fires an error if there are some whitespace garbage between the object or after it
echo5 = (stdinEnum $$ (E.sequence (iterParser jsonOrEOF)
                        =$ EL.isolateWhile isRight
                        =$ EL.map (\(Right v) -> v)
                        =$ EL.mapM dispatch
                        =$ EL.map (BS.concat . BSL.toChunks . AE.encode)
                        =$ EB.iterHandle stdout))
  where
    -- this is just a dumb echoer, must be some wiser one also
    -- maybe also -- there can be asynchronous messages
    -- so there must be something smarted than mapM
    -- (when it comes that far)
    dispatch = return

echo6 = (stdinEnum $$ (E.sequence (iterParser jsonOrEOF)
                        =$ EL.isolateWhile isRight
                        =$ EL.map (\(Right v) -> v)
                        =$ EL.mapM_ dispatch))
  where
    dispatch v = liftIO $ BSL.hPut stdout (AE.encode v)

isRight (Left _) = False
isRight (Right _) = True
