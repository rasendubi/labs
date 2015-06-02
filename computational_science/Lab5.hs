{-# LANGUAGE QuasiQuotes #-}
module Lab5 (main) where

import Data.List

import Graphics.EasyPlot

import qualified Data.String.Here as H

args :: [Double]
args = [-6, -4 .. 2]

values :: [Double]
values = fmap exactFunction args

points :: [(Double, Double)]
points = toPoints exactFunction args

exactFunction :: Double -> Double
exactFunction x = x^(2::Int) / 15 + cos (x + 2)

toPoints :: (a -> b) -> [a] -> [(a, b)]
toPoints f = fmap $ \x -> (x, f x)

main :: IO ()
main = do
    putStr "x = " >> print args
    putStr "y = " >> print values

    putStr "Lagrange polynom: P(x) = " >> putStrLn (showLagrangePolynom args values)
    putStrLn ""
    putStrLn "Splines: S(x) =" >> putStrLn (showSpline points)

    let range = Range (head args - 1) (last args + 1)
    plot X11
        [ Data2D [Title ""] [range] points
        , Function2D [Title "x^2 / 15 + cos(x+2)"] [range] exactFunction
        , Function2D [Title "lagrange"] [range] (lagrange args values)
        , Function2D [Title "spline"] [range] (spline points)
        ]

    return ()

spline :: (Ord a, Fractional a) => [(a, a)] -> a -> a
spline points = \x ->
        let
            idx = min (length coefs - 1) . length . drop 1 . takeWhile (< x) $ fmap fst points
            (x0, y0) = points !! idx
            (bs, cs, ds) = coefs !! idx
            dx = x - x0
        in y0 + (bs + (cs/2 + ds*dx/6)*dx)*dx
    where
        coefs = splineCoefs points

splineCoefs :: Fractional a => [(a, a)] -> [(a, a, a)]
splineCoefs points = zip3 bs cs ds
    where
        alpha_beta = scanl findCoeffs (0, 0) $ zip3 points (tail points) (tail $ tail points)
        findCoeffs (alpha, beta) ((x0, y0), (x1, y1), (x2, y2)) =
            let
                a = x1 - x0
                b = x2 - x1
                c = 2 * (a + b)
                f = 6 * ((y2 - y1)/b - (y1 - y0)/a)
                divider = a * alpha + c
            in (-b/divider, (f - a*beta)/divider)
        cs = 0 : reverse (scanl findCs 0 (reverse $ tail alpha_beta))
        findCs cs (alpha, beta) = alpha*cs + beta
        (ds, bs) = unzip $ zipWith4 findDsBs points (tail points) cs (tail cs)
        findDsBs (x0, y0) (x1, y1) cs0 cs1 = ( (cs1 - cs0)/hi, (y1 - y0)/hi - hi*(cs1 + 2*cs0)/6 )
            where hi = x1 - x0

showSpline :: [(Double, Double)] -> String
showSpline points = unlines $ zip3With points (tail points) coefs $ \(x0, y0) (x1, _) (cs, bs, ds) ->
        [H.i|${y0} + (${bs})*${showTerm x0} + (${cs}/2)*${showTerm x0}^2 + (${ds}/6)*${showTerm x0}^3 | for x in [${x0}; ${x1}] |]
    where coefs = splineCoefs points

lagrange :: Fractional a => [a] -> [a] -> a -> a
lagrange xs ys = \x -> sum $ zip3With ys (lagrangeNumers x xs) denums $ \y numer denum ->
        y * numer / denum
    where denums = lagrangeDenums xs

lagrangeNumers :: Num b => b -> [b] -> [b]
lagrangeNumers x = fmap (\(_, xs) -> product xs) . splitEvery' . fmap (\xi -> x - xi)

lagrangeDenums :: Num a => [a] -> [a]
lagrangeDenums = fmap f . splitEvery'
    where f (x, xs) = product $ fmap (\xi -> x - xi) xs

-- newton :: Fractional b => [b] -> [b] -> [b]
-- newton args = fmap head . take (length args) . iterate (getFs args)
--     where
--         getFs args prev = zipWith4 f args prev (drop (length args - length prev + 1) args) (tail prev)
--             where f arg1 value1 arg2 value2 = (value2 - value1) / (arg2 - arg1)

showLagrangePolynom :: (Show a1, Show a, Ord a1, Num a1) => [a1] -> [a] -> [Char]
showLagrangePolynom xs ys = intercalate " + " $ zip3With ys (showLagrangeNumers xs) denums $ \y numer denum ->
        show y ++ "*" ++ numer ++ "/" ++ show denum
    where
        denums = lagrangeDenums xs

showLagrangeNumers :: (Show a, Ord a, Num a) => [a] -> [String]
showLagrangeNumers = fmap (\(_, xs) -> intercalate "*" xs) . splitEvery' . fmap showTerm

showTerm :: (Show a, Ord a, Num a) => a -> [Char]
showTerm x
    | x == 0    = "x"
    | x > 0     = "(x - " ++ show x ++ ")"
    | otherwise = "(x + " ++ show (-x) ++ ")"

splitEvery' :: [a] -> [(a, [a])]
splitEvery' [] = []
splitEvery' (x:xs) = (x, xs) : fmap prepend_x (splitEvery' xs)
    where prepend_x (j, k) = (j, x:k)

zip3With :: [a] -> [b] -> [c] -> (a -> b -> c -> d) -> [d]
zip3With x y z f = zipWith3 f x y z
