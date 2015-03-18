{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
module Main (main) where

import Prelude hiding (mapM_)

import qualified Data.Vector as V
import           Data.Vector (Vector, (!), mapM_, forM_)
import Data.Function (on)
import Data.List

import Control.Applicative ((<$>))
import qualified Control.Monad as M

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

residual a x = diff $ calculate a x

mean :: [Double] -> Double
mean = uncurry (/) . foldl' (\(s,!c) e -> (e+s,c+1)) (0,0)

sigma :: Vector Double -> Vector Double -> Double
sigma x y = sqrt . mean . map (^2) $ zipWith (-) (V.toList x) (V.toList y)

a2 :: Matrix Double
a2 = V.fromList $ map V.fromList
    [ [ 7.03,   1.22, 0.85, -0.81, 1.135 ]
    , [ 0.98,   3.39, 1.3,   0.57, -1.63 ]
    , [ 1.09,  -2.46, 6.21, 1.033,  2.1  ]
    , [ 1.345,  0.16, 2.1,  -12,    5.33 ]
    , [ 1.29,  -1.23, -0.767, 1,    6    ]
    ]

b2 = V.fromList [ 2.1, 0.84, 2.58, 11.96, -1.47 ]

sum' :: Num a => Int -> Int -> (Int -> a) -> a
sum' from to f = sum $ f <$> [from..to]

zeidelStep :: Matrix Double -> Vector Double -> Vector Double -> Vector Double
zeidelStep a b xk =
    let n = V.length xk
        x = V.generate n $ \i ->
            - sum' 0 (i-1) (\j -> x!j * a!i!j / a!i!i)
            - sum' (i+1) (n-1) (\j -> xk!j * a!i!j / a!i!i)
            + b!i / a!i!i
    in x

diff :: Vector Double -> Vector Double -> Vector Double
diff = V.zipWith ((abs .) . (-))

converges :: Double -> Vector Double -> Vector Double -> Bool
converges eps x y = V.maximum (diff x y) >= eps

-- iterate $ zeidelStep a2 b2 $ V.fromList [1,2,3,4,5]

takeWhile' :: (a -> a -> Bool) -> [a] -> [a]
takeWhile' _ []  = []
takeWhile' _ [x] = [x]
takeWhile' p (x:y:ys)
    | p x y     = x : takeWhile' p (y:ys)
    | otherwise = [x]

main :: IO ()
main = do
    let x_octave = V.fromList
            [ 0.1045500288835936
            , 0.1279690837540869
            , 0.6008089385982676
            , -0.8855821467426077
            , -0.0168441602658050
            ]

    let x_init = V.fromList [1,2,3,4,5]
    let iters = takeWhile' (converges 1.0e-6) $ iterate (zeidelStep a2 b2) x_init
    let x_last = last iters

    M.forM_ iters $ \x -> do
        putStr "x        = " >> printRow x
        putStr "residual = " >> printRow (residual a2 x b2)
        putStrLn ""
    putStr "sigma = " >> print (sigma x_last x_octave)

-- main :: IO ()
-- main = do
--     let x = gauss a b
--     let r = residual a x b
--     let refresult = V.fromList
--             [ 1.180177639024360
--             , -0.562950447022865
--             , 1.359449570301241
--             , -1.077619795950106
--             ]
--     let s = sigma x refresult
--     putStr "x = " >> print (V.toList x)
--     putStr "r = " >> print (V.toList r)
--     putStr "s = " >> print s
