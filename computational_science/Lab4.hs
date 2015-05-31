module Lab4 (main) where

import Utils

import Control.Monad

import qualified Data.Vector as V

data Segment a = Segment a a deriving (Eq, Show)

polynomial :: V.Vector Double
polynomial = V.fromList [2, 2, 0, -4, -2, 3]

segments :: [Segment Double]
segments = [Segment (-7/3) (-1/3)]

main :: IO ()
main = do
    runMethod "Bisection" bisection
    runMethod "Chords" chords
    runMethod "Newton" $ \eps p (Segment a b) -> newton eps p ((a+b)/2)
    where 
        runMethod name method = do
            putStrLn $ name ++ " method:"
            forM_ segments $ \segment -> do
                print segment
                let results = method 1.0e-6 polynomial segment
                forM_ results $ \res -> print res
                putStr "Steps: " >> print (length results)
                putStr "Result: "
                print $ last results
                putStrLn ""

bisection :: (Floating a, Ord a) => a -> V.Vector a -> Segment a -> [Segment a]
bisection eps p = iterateWhile (approxNext (const bisection') p) shouldContinue
    where shouldContinue _ (Segment a b) = abs (value p $ bisection' a b) > eps || abs (b - a) > eps

chords :: (Floating a, Ord a) => a -> V.Vector a -> Segment a -> [Segment a]
chords eps p = iterateWhile (approxNext chords' p) shouldContinue
    where
        shouldContinue (Segment prevA prevB) (Segment curA curB) =
                abs (curX - prevX) > eps || value p curX > eps
            where
                prevX = chords' (value p) prevA prevB
                curX = chords' (value p) curA curB

newton :: (Fractional a, Ord a) => a -> V.Vector a -> a -> [a]
newton eps p = iterateWhile (newton' p) shouldContinue
    where shouldContinue prevX curX = abs (curX - prevX) > eps || value p curX > eps

bisection' :: (Floating a, Ord a) => a -> a -> a
bisection' a b = (a + b) / 2

chords' :: (Floating a, Ord a) => (a -> a) -> a -> a -> a
chords' f a b = (a * f b - b * f a)/(f b - f a)

newton' :: (Fractional a) => V.Vector a -> a -> a
newton' p x = x - f x / f' x
    where
        f  = value  p
        f' = value' p

approxNext :: (Num a, Ord a) => ((a -> a) -> a -> a -> a) -> V.Vector a -> Segment a -> Segment a
approxNext guess p (Segment a b) = if f a * f c > 0 then Segment c b else Segment a c
    where
        f = value p
        c = guess f a b

value :: Num a => V.Vector a -> a -> a
value v x = V.sum $ V.imap (\i k -> k * x^i) v

value' :: Num a => V.Vector a -> a -> a
value' v x = V.sum $ V.imap (\i k -> fromIntegral (i+1) * k * x^i) $ V.tail v

iterateWhile :: (a -> a) -> (a -> a -> Bool) -> a -> [a]
iterateWhile f p = takeWhile' p . iterate f
