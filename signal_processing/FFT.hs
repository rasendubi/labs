module FFT where

import Prelude hiding (length, sum, map, zipWith, (++), foldr, concat)
import qualified Prelude as P

import Data.Bits
import Data.Complex
import Data.Vector

bitrev :: Int -> Vector Int
bitrev n = generate n (\i -> foldl' (onebit i) 0 bs)
    where
        nbits = log2 n
        bs = generate nbits id
        onebit i r b
            | testBit i b = setBit r $ nbits - 1 - b
            | otherwise   = r

log2 :: Int -> Int
log2 1 = 0
log2 n = 1 + log2 (n `div` 2)

fft :: Vector (Complex Double) -> Vector (Complex Double)
fft = fft' 1 1

ifft :: Vector (Complex Double) -> Vector (Complex Double)
ifft v = fft' (-1) (1.0 / fromIntegral (length v)) v

fft' :: Int -> Double -> Vector (Complex Double) -> Vector (Complex Double)
fft' sign scale h =
    if n <= 2
        then dft' sign scale h
        else map ((scale :+ 0) *) $ recomb $ backpermute h (bitrev n)
    where
        n = length h
        recomb = foldr (.) id $ map dl $ iterateN (log2 n) (`div` 2) n
        dl m v = let w = omega m
                     m2 = m `div` 2
                     ds = map ((w ^^) . (sign *)) $ enumFromN 0 m2
                     doone v = let v0 = slice 0 m2 v
                                   v1 = zipWith (*) ds $ slice m2 m2 v
                               in zipWith (+) v0 v1 ++ zipWith (-) v0 v1
                 in concat $ P.map doone $ slicevecs m v
        slicevecs m v = P.map (\i -> slice (i * m) m v) [0..n `div` m - 1]

i :: Complex Double
i = 0 :+ 1

omega :: Int -> Complex Double
omega n = cis (2 * pi / fromIntegral n)
 
dft :: Vector (Complex Double) -> Vector (Complex Double)
dft = dft' 1 1

idft :: Vector (Complex Double) -> Vector (Complex Double)
idft v = dft' (-1) (1.0 / (fromIntegral $ length v)) v
 
dft' :: Int -> Double -> Vector (Complex Double) -> Vector (Complex Double)
dft' sign scale h = generate bigN (((scale :+ 0) *) . doone)
  where bigN = length h
        w = omega bigN
        doone n = sum $
                  zipWith (*) h $ generate bigN (\k -> w^^(sign*n*k))
