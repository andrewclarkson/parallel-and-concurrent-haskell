import RSA (decrypt, public)
import qualified Data.ByteString.Lazy.Char8 as ByteString

main :: IO ()
main = do
    message <- ByteString.getContents
    ByteString.putStr $ decrypt public message
