module TestClient where

import qualified RPC
import System.Process (CmdSpec(RawCommand))
import System.IO.Unsafe (unsafePerformIO)

makeConnection = RPC.newConnectionCommand False (RawCommand "./TestServer" [])

main =
  do
    c <- makeConnection
    let mIO = RPC.callMethod c "exp" :: Double -> IO Double
    let m v = unsafePerformIO (mIO v)
    putStrLn ("m 1 = " ++ show (m 1))
    putStrLn ("m 10 = " ++ show (m 10))
    putStrLn ("m -1 = " ++ show (m (-1)))
