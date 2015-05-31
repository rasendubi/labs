{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Utils
    ( Matrix
    , Vector
    , mkMatrix
    , (V.!)
    , (<$>)
    , on
    , swap
    , fromMatrix
    , showMatrix
    , printMatrix
    , printRow
    , diff
    , residual
    , mean
    , sigma
    , identity
    , mul
    , for
    , takeWhile'
    ) where

import Prelude hiding (mapM_)

import Data.Function (on)

import qualified Data.Vector as V
import           Data.Vector (Vector, mapM_, forM_, (!))

import Data.List

import Text.Printf

import Control.Applicative ((<$>))

type Matrix a = Vector (Vector a)

fromMatrix :: Matrix a -> [[a]]
fromMatrix = fmap V.toList . V.toList

mkMatrix :: [[a]] -> Matrix a
mkMatrix = V.fromList . fmap V.fromList

swapInList :: Int -> Int -> [Int] -> [Int]
swapInList x y = fmap $ \case
    e | e == x    -> y
      | e == y    -> x
      | otherwise -> e

swap :: Int -> Int -> Vector a -> Vector a
swap x y v = V.backpermute v . V.fromList $ swapInList x y [0 .. V.length v - 1]

showMatrix :: Show a => Matrix a -> String
showMatrix = intercalate "\n" . fmap (intercalate "\t" . fmap show) . fromMatrix

printMatrix :: PrintfArg a => Matrix a -> IO ()
printMatrix = mapM_ printRow

printRow :: PrintfArg a => Vector a -> IO ()
printRow v = do
    forM_ v $ \e -> printf "%-10.6f " e
    putStrLn ""

residual a x = diff $ calculate a x
    where
        calculate :: Matrix Double -> Vector Double -> Vector Double
        calculate a x = V.map (V.sum . V.zipWith (*) x) a

mean :: [Double] -> Double
mean = uncurry (/) . foldl' (\(s,!c) e -> (e+s,c+1)) (0,0)

sigma :: Vector Double -> Vector Double -> Double
sigma x y = sqrt . mean . map (^2) $ zipWith (-) (V.toList x) (V.toList y)

diff :: Vector Double -> Vector Double -> Vector Double
diff = V.zipWith ((abs .) . (-))

identity :: Num a => Int -> Matrix a
identity n =
    V.generate n $ \i ->
        V.generate n $ \j ->
            toNum $ i == j

toNum :: Num a => Bool -> a
toNum True  = 1
toNum False = 0

mul :: Matrix Double -> Matrix Double -> Matrix Double
mul x y =
        V.generate len $ \i ->
            V.generate len $ \j ->
                sum $ for [0 .. len-1] $ \k ->
                    x!i!k * y!k!j
    where len = V.length x

for :: Functor f => f a -> (a -> b) -> f b
for = flip fmap

takeWhile' :: (a -> a -> Bool) -> [a] -> [a]
takeWhile' _ []  = []
takeWhile' _ [x] = [x]
takeWhile' p (x:y:ys)
    | p x y     = x : takeWhile' p (y:ys)
    | otherwise = [x]
