{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Prelude hiding (mapM_)

import qualified Data.Vector as V
import           Data.Vector (Vector, (!), mapM_, forM_)
import Data.Function (on)
import Data.List

import Text.Printf

type Matrix a = Vector (Vector a)

a :: Matrix Double
a = V.fromList $ map V.fromList
    [ [ 3.81, 0.25, 1.28, 1.75 ]
    , [ 2.25, 1.32, 5.58, 0.49 ]
    , [ 5.31, 7.28, 0.98, 1.04 ]
    , [ 10.39,2.45, 3.35, 2.28 ]
    ]

b :: Vector Double
b = V.fromList [ 4.21, 8.97, 2.38, 12.98 ]

maximumRow :: Ord a => Int -> Matrix a -> Int
maximumRow num m = (num +) . V.maxIndexBy (compare `on` (V.! num)) . V.slice num (V.length m - num) $ m

swapInList :: Int -> Int -> [Int] -> [Int]
swapInList x y = fmap $ \case 
    e | e == x    -> y
      | e == y    -> x
      | otherwise -> e

swap :: Int -> Int -> Vector a -> Vector a
swap x y v = V.backpermute v . V.fromList $ swapInList x y [0 .. V.length v - 1]

concatMatrix :: Matrix a -> Vector a -> Matrix a
concatMatrix = V.zipWith V.snoc

gauss :: RealFrac a => Matrix a -> Vector a -> Vector a
gauss a b = V.imap (\i row -> V.last row / row!i) $ backward $ forward $ concatMatrix a b

forward :: RealFrac a => Matrix a -> Matrix a
forward = forward' 0
    where
        forward' :: RealFrac a => Int -> Matrix a -> Matrix a
        forward' n ab
            | n == V.length ab = ab
            | otherwise        = forward' (n+1) $
                let maxRow = maximumRow n ab
                    swappedRows = swap n maxRow ab
                    refRow = swappedRows ! n
                in flip V.imap swappedRows $ \i row ->
                        if i <= n then row
                                  else let k = row!n / refRow!n
                                       in V.izipWith (f n k) refRow row
        f :: Num a => Int -> a -> (Int -> a -> a -> a)
        f n k i a b
            | i <= n    = 0
            | otherwise = b - k*a

backward :: RealFrac a => Matrix a -> Matrix a
backward ab = backward' (V.length ab - 1) ab
    where
        backward' :: RealFrac a => Int -> Matrix a -> Matrix a
        backward' (-1) ab = ab
        backward' n ab = backward' (n-1) $
            let refRow = ab ! n
            in flip V.imap ab $ \i row ->
                if i >= n then row
                          else let k = row!n / refRow!n
                               in V.izipWith (g n k) refRow row
        g :: Num a => Int -> a -> (Int -> a -> a -> a)
        g n k i x y
            | i >= n && i /= V.length ab = 0
            | otherwise                  = y - k*x
            
showMatrix = V.toList . V.map V.toList

printMatrix :: PrintfArg a => Matrix a -> IO ()
printMatrix = mapM_ printRow

printRow :: PrintfArg a => Vector a -> IO ()
printRow v = do
    forM_ v $ \e -> printf "%-10.6f " e
    putStrLn ""

calculate :: Matrix Double -> Vector Double -> Vector Double
calculate a x = V.map (V.sum . V.zipWith (*) x) a

residual a x = V.zipWith ((abs .) . (-)) $ calculate a x

mean :: [Double] -> Double
mean = uncurry (/) . foldl' (\(s,!c) e -> (e+s,c+1)) (0,0)

sigma :: Vector Double -> Vector Double -> Double
sigma x y = sqrt . mean . map (^2) $ zipWith (-) (V.toList x) (V.toList y)

main :: IO ()
main = do
    let x = gauss a b
    let r = residual a x b
    let refresult = V.fromList
            [ 1.180177639024360
            , -0.562950447022865
            , 1.359449570301241
            , -1.077619795950106
            ]
    let s = sigma x refresult
    putStr "x = " >> print (V.toList x)
    putStr "r = " >> print (V.toList r)
    putStr "s = " >> print s
