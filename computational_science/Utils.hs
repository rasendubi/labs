{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Utils
    ( Matrix
    , Vector
    , (V.!)
    , (<$>)
    , on
    , swap
    , printMatrix
    , printRow
    , diff
    , residual
    , mean
    , sigma
    ) where

import Prelude hiding (mapM_)

import Data.Function (on)

import qualified Data.Vector as V
import           Data.Vector (Vector, mapM_, forM_)

import Data.List

import Text.Printf

import Control.Applicative ((<$>))

type Matrix a = Vector (Vector a)

swapInList :: Int -> Int -> [Int] -> [Int]
swapInList x y = fmap $ \case
    e | e == x    -> y
      | e == y    -> x
      | otherwise -> e

swap :: Int -> Int -> Vector a -> Vector a
swap x y v = V.backpermute v . V.fromList $ swapInList x y [0 .. V.length v - 1]

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
