module Lab6 (main) where

function :: Double -> Double
function x = logBase 10 (x^2 + 1) / x

a, b :: Double
(a, b) = (0.7, 1.4)

main :: IO ()
main = do
    let real = 0.211588
    putStr "Real value: " >> print real

    putStrLn ""

    let simp_n = 2
    let simp = simpson (a,b) ((b - a)/simp_n) function
    putStr "Simpson method: " >> print simp
    putStr "N = " >> print simp_n
    putStr "Accuracy: " >> print (abs $ real - simp)

    putStrLn ""

    let gaus_n = 2
    let gaus = gaussian (rules!!(gaus_n - 1)) (a,b) function
    putStr "Gaussian quadrature: " >> print gaus
    putStr "N = " >> print gaus_n
    putStr "Accuracy: " >> print (abs $ real - gaus)

simpson :: (Double, Double) -> Double -> (Double -> Double) -> Double
simpson (a, b) h f = h/3 * sum (map (\(y0,y1,y2) -> y0 + 4*y1 + y2) (skipEverySecond $ every3 ys))
    where
        ys = fmap f [a, a + h .. b]

gaussian :: [(Double, Double)] -> (Double, Double) -> (Double -> Double) -> Double
gaussian rule (a, b) f = (b - a)/2 * sum (fmap (\(x, w) -> w * f' x) rule)
    where f' x = f $ ( (b - a)*x + a + b )/2

rules :: [[(Double, Double)]]
rules =
    [ [ (0, 2)
      ]

    , [ (- sqrt (1/3), 1)
      , (  sqrt (1/3), 1)
      ]

    , [ (0, 8/9)
      , (- sqrt (3/5), 5/9)
      , (  sqrt (3/5), 5/9)
      ]

    , [ (- sqrt (3/7 - 2/7 * sqrt (6/5)), (18 + sqrt 30)/36)
      , (  sqrt (3/7 - 2/7 * sqrt (6/5)), (18 + sqrt 30)/36)
      , (- sqrt (3/7 + 2/7 * sqrt (6/5)), (18 - sqrt 30)/36)
      , (  sqrt (3/7 + 2/7 * sqrt (6/5)), (18 - sqrt 30)/36)
      ]

    , [ (0, 128/225)
      , (- 1/3 * sqrt (5 - 2 * sqrt (10/7)), (322 + 13 * sqrt 70)/900)
      , (  1/3 * sqrt (5 - 2 * sqrt (10/7)), (322 + 13 * sqrt 70)/900)
      , (- 1/3 * sqrt (5 + 2 * sqrt (10/7)), (322 - 13 * sqrt 70)/900)
      , (  1/3 * sqrt (5 + 2 * sqrt (10/7)), (322 - 13 * sqrt 70)/900)
      ]

    ]

every3 :: [a] -> [(a,a,a)]
every3 (a: tail@(b:c:_)) = (a,b,c) : every3 tail
every3 _ = []

skipEverySecond :: [a] -> [a]
skipEverySecond (x:_:ys) = x : skipEverySecond ys
skipEverySecond x = x
