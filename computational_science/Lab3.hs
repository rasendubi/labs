{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}
module Lab3 (main) where

import Utils

import Data.List
import qualified Data.Vector as V
import           Data.Vector ((//))

import Debug.Trace
import qualified Data.String.Here as H

main :: IO ()
main = do
    putStrLn "A ="
    printMatrix a
    putStrLn ""
    putStrLn "P ="
    printMatrix $ danilevsky a

a = mkMatrix
    [ [6.26 + a, 1.10 - b, 0.97 + g, 1.24 - d]
    , [1.10 - b, 4.16 - a,     1.30,     0.16]
    , [0.97 + g, 1.30,     5.44 + a,     2.10]
    , [1.24 - d, 0.16,         2.10, 6.10 - a]
    ]
    where
        t = 1
        k = 3 * (2 - 4) + 1
        a = 0.11 * t
        b = 0.02 * k
        d = 0.015 * t
        g = 0.02 * k

danilevsky :: Matrix Double -> Matrix Double
danilevsky m = iteration (V.length m - 1) m

-- i = m - i + 1
iteration :: Int -> Matrix Double -> Matrix Double
iteration 0 a = a
iteration i (normalize i -> Just a) = iteration (i-1) $ m' `mul` a `mul` m
        `ftrace`
            [H.i|
Iteration ${i}
M  =
${showMatrix m}

M' =
${showMatrix m'}

a  =
${showMatrix a}
            |]
    where
        m = identity len // [(i - 1, V.generate len $ \j -> if j /= i - 1
                then -a!i!j / a!i!(i-1)
                else 1 / a!i!(i-1)
            )]
        m' = identity len // [(i - 1, V.generate len $ \j -> a!i!j)]
        len = V.length a
iteration i a = traceShowId $ mergeTopLeft a $ danilevsky $ topLeft i a

ftrace = flip trace

normalize :: Int -> Matrix Double -> Maybe (Matrix Double)
normalize i a =
    if a!i!(i-1) /= 0
        then return a
        else (\swapIndex -> swapRowCol i swapIndex a) `fmap` getSwapIndex a i

swapRowCol :: Int -> Int -> Matrix a -> Matrix a
swapRowCol x y = swap x y . fmap (swap x y)

getSwapIndex :: (Eq a, Num a) => Matrix a -> Int -> Maybe Int
getSwapIndex m i = flip find [0 .. i-1] $ \x -> m!i!x /= 0
{-# SPECIALIZE getSwapIndex :: Matrix Double -> Int -> Maybe Int #-}

topLeft :: Int -> Matrix a -> Matrix a
topLeft i = fmap (V.slice 0 i) . V.slice 0 i

mergeTopLeft :: Matrix a -> Matrix a -> Matrix a
mergeTopLeft x tl =
    V.generate x_length $ \i ->
        V.generate x_length $ \j ->
            if i < tl_length && j < tl_length
                then tl!i!j
                else x!i!j
    where
        x_length = V.length x
        tl_length = V.length tl
