import RSA (encrypt, private)
import qualified Data.ByteString.Lazy.Char8 as ByteString

main :: IO ()
main = do
    message <- ByteString.getContents
    ByteString.putStr $ encrypt private message
