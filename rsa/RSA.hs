module RSA (
    RSAKey(..),
    encrypt,
    encryptStream,
    decrypt,
    decryptStream,
    public,
    private,
    chunk,
    size,
    integer
) 
where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.ByteString.Lazy.Char8 (ByteString)

import Control.Parallel.Strategies (withStrategy, parBuffer, rdeepseq)
import Control.Monad.Par (Par)

import Stream (Stream, streamMap) 

-- Numbers taken from the Wikipedia example
n = 3233 :: Integer
d = 2753 :: Integer
e = 17 :: Integer

data RSAKey = RSAKey Integer Integer deriving (Show)

private = RSAKey n d
public = RSAKey n e

encrypt :: RSAKey -> ByteString -> ByteString
encrypt (RSAKey n e) = ByteString.unlines
            . withStrategy (parBuffer 100 rdeepseq)
            . map (ByteString.pack . show . power e n . code )
            . chunk (size n)

encryptStream :: RSAKey -> Stream ByteString -> Par (Stream ByteString)
encryptStream (RSAKey n e) s = streamMap (ByteString.pack . show . power e n . code) s 
                      
decrypt :: RSAKey -> ByteString -> ByteString
decrypt (RSAKey n d) = ByteString.concat
            . map (ByteString.pack . decode . power d n)
            . integers
            . ByteString.lines

decryptStream :: RSAKey -> Stream ByteString -> Par (Stream ByteString)
decryptStream (RSAKey n d) s = streamMap (ByteString.pack . decode . power d n . integer) s

integers :: [ByteString] -> [Integer]
integers bs = [ i | Just (i,_) <- map ByteString.readInteger bs ]

integer :: ByteString -> Integer
integer b | Just (i,_) <- ByteString.readInteger b = i

code :: ByteString -> Integer
code = ByteString.foldl' accum 0
  where accum x y = (128 * x) + fromIntegral (fromEnum y)

decode :: Integer -> String
decode n = reverse (expand n)
   where expand 0 = []
         expand x = toEnum (fromIntegral (x `mod` 128)) : expand (x `div` 128)

chunk :: Int -> ByteString -> [ByteString]
chunk n xs | ByteString.null xs = []
chunk n xs = as : chunk n bs
  where (as,bs) = ByteString.splitAt (fromIntegral n) xs

size :: Integer -> Int
size n = (length (show n) * 47) `div` 100

power :: Integer -> Integer -> Integer -> Integer
power 0 m x          = 1
power n m x | even n = sqr (power (n `div` 2) m x) `mod` m
	    | True   = (x * power (n-1) m x) `mod` m

sqr :: Integer -> Integer
sqr x = x * x
