module Lab2 (main) where

import qualified Control.Monad as M
import qualified Data.Vector as V

import Utils

a2 :: Matrix Double
a2 = V.fromList $ map V.fromList
    [ [ 7.03,   1.22, 0.85, -0.81, 1.135 ]
    , [ 0.98,   3.39, 1.3,   0.57, -1.63 ]
    , [ 1.09,  -2.46, 6.21, 1.033,  2.1  ]
    , [ 1.345,  0.16, 2.1,  -12,    5.33 ]
    , [ 1.29,  -1.23, -0.767, 1,    6    ]
    ]

b2 :: Vector Double
b2 = V.fromList [ 2.1, 0.84, 2.58, 11.96, -1.47 ]

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

zeidelStep :: Matrix Double -> Vector Double -> Vector Double -> Vector Double
zeidelStep a b xk =
    let n = V.length xk
        x = V.generate n $ \i ->
            - sum' 0 (i-1) (\j -> x!j * a!i!j / a!i!i)
            - sum' (i+1) (n-1) (\j -> xk!j * a!i!j / a!i!i)
            + b!i / a!i!i
    in x

converges :: Double -> Vector Double -> Vector Double -> Bool
converges eps x y = V.maximum (diff x y) >= eps

takeWhile' :: (a -> a -> Bool) -> [a] -> [a]
takeWhile' _ []  = []
takeWhile' _ [x] = [x]
takeWhile' p (x:y:ys)
    | p x y     = x : takeWhile' p (y:ys)
    | otherwise = [x]

sum' :: Num a => Int -> Int -> (Int -> a) -> a
sum' from to f = sum $ f <$> [from..to]
