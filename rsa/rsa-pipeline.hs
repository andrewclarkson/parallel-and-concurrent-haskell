import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.ByteString.Lazy.Char8 (ByteString)

import Control.Monad.Par (runPar)

import RSA 
import Stream (streamFromList, streamFold)

pipeline :: ByteString -> ByteString
pipeline bs = runPar $ do
    let (RSAKey n _) = public
    s0 <- streamFromList (chunk (size n) bs)
    s1 <- encryptStream public s0
    s2 <- decryptStream private s1
    xs <- streamFold (\x y -> (y : x)) [] s2
    return (ByteString.unlines (reverse xs))

main :: IO ()
main = do
    message <- ByteString.getContents
    ByteString.putStr $ pipeline message
