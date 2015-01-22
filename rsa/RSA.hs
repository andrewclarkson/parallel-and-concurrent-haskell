module RSA (
    encrypt,
    decrypt,
    public,
    private
) 
where

import qualified Data.ByteString.Lazy.Char8 as ByteString
import Data.ByteString.Lazy.Char8 (ByteString)

import Control.Parallel.Strategies (withStrategy, parList, rdeepseq)

-- Numbers taken from the Wikipedia example
n = 3233 :: Integer
d = 2753 :: Integer
e = 17 :: Integer

data Key = Key !Integer !Integer

private = Key n d
public = Key n e

encrypt :: Key -> ByteString -> ByteString
encrypt (Key n e) = ByteString.unlines
            . withStrategy (parList rdeepseq)
            . map (ByteString.pack . show . power e n . code )
            . chunk (size n)
                      
decrypt :: Key -> ByteString -> ByteString
decrypt (Key n d) = ByteString.concat
            . map (ByteString.pack . decode . power d n)
            . integers
            . ByteString.lines

integers :: [ByteString] -> [Integer]
integers bs = [ i | Just (i,_) <- map ByteString.readInteger bs ]

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
