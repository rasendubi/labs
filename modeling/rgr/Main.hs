{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import Control.Monad (msum, forM_, mapM_)
import Control.Monad.ST (runST)

import qualified Data.Csv as Csv
import           Data.Vector (Vector)
import qualified Data.Vector as V
import qualified Data.Map as M
import qualified Data.Vector.Algorithms.Intro as V (sort)
import qualified Data.ByteString.Lazy as BL

import Numeric (showFFloat)

import Data.Maybe (fromMaybe, fromJust)

import Numeric.SpecFunctions (logGamma)

import System.Environment (getArgs)

type Row = (String, Double, Double, Double, Double, Integer)

main :: IO ()
main = do
  [file] <- getArgs
  Right (rows :: Vector Row) <- Csv.decode Csv.NoHeader <$> BL.readFile file
  let d = fmap (\(_,_,_,_,x,_) -> x) rows
  mapM_ print d
  let alphabets = [10, 15, 20, 26]
  let nDiffers = 6
  let tables = buildTables (calc d alphabets nDiffers) alphabets
  forM_ tables $ \row ->
      forM_ row $ \table -> do
          forM_ (M.toList table) $ \((f,t), p) ->
              putStrLn $ toAlpha f ++ "--(" ++ showFFloat (Just 4) p "" ++ ")-->" ++ toAlpha t
          putStrLn ""

toAlpha :: Int -> String
toAlpha = show . ((['A' .. 'Z'] ++ ['a' .. 'z'] ++ ['?','!','#']) !!)

buildTables
  :: [(Vector Double, Vector Double, Vector Double, Vector Double, [Vector Double], [Vector Double])] -> [Int] -> [[M.Map (Int, Int) Double]]
buildTables xs alphabets =
    flip fmap xs $ \(sorted, unsorted, negative, positive, negative', positive') ->
        flip fmap (zip3 alphabets negative' positive') $ \(alpha, neg, pos) ->
            let
                negConverted = convertCount (V.length negative) neg
                partsNeg = divideElementsIntoParts negative negConverted
                posConverted = convertCount (V.length positive) pos
                partsPos = divideElementsIntoParts positive posConverted
                alphabetSeries = convertToAlphabet partsNeg partsPos unsorted
                probabilitiesTable = calcProbabilities alphabetSeries (length partsNeg + length partsPos)
            in probabilitiesTable

    where
      convertCount :: Int -> Vector Double -> [Int]
      convertCount len counts = V.toList result
          where
            sumCounts = V.sum counts
            correctionMultiply = fromIntegral len / sumCounts
            result' = fmap (\x -> floor $ x*correctionMultiply) counts
            sumResult = sum result'
            correctionDiff = len - sumResult
            maxIndexResult = V.maxIndex result'
            result = result' V.// [(maxIndexResult, (result' V.! maxIndexResult) + correctionDiff)]
      divideElementsIntoParts :: Vector a -> [Int] -> [Vector a]
      divideElementsIntoParts _      []     = []
      divideElementsIntoParts series (x:xs) = t : divideElementsIntoParts d xs
          where (t,d) = V.splitAt x series

      convertToAlphabet :: [Vector Double] -> [Vector Double] -> Vector Double -> Vector Int
      convertToAlphabet partsNeg partsPos unsorted =
          flip fmap unsorted $ \series ->
              let
                  neg = findElementInParts series partsNeg
                  pos = findElementInParts series partsPos
              in fromMaybe (fromMaybe (length partsPos) pos + length partsNeg) neg

      findElementInParts :: Double -> [Vector Double] -> Maybe Int
      findElementInParts x parts = msum $ zipWith f [0 .. ] parts
          where
            f i part
                | V.null part     = Nothing
                | V.last part > x = Just i
                | otherwise       = Nothing

      calcProbabilities :: Vector Int -> Int -> M.Map (Int, Int) Double
      calcProbabilities series len = M.mapWithKey (\k i -> fromIntegral i / fromIntegral (lets M.! fst k)) arcs
          where
            arcs = calcProb $ V.toList $ V.zip series (V.tail series)
            lets = calcProb $ V.toList $ V.init series
            calcProb xs = M.fromListWith (+) [(x, 1) | x <- xs]

calc :: Vector Double
     -> [Int]
     -> Double
     -> [(Vector Double,
          Vector Double,
          Vector Double,
          Vector Double,
          [Vector Double],
          [Vector Double])]
calc _ _ 0 = []
calc xs alphabets nDiffers = (sorted, unsorted, negative, positive, negative', positive') : calc unsorted alphabets (nDiffers - 1)
    where
      unsorted = diff xs
      sorted = sort unsorted
      (negative, positive) = V.partition (< 0) sorted
      negative' = calcDistribution (student 10) negative <$> alphabets
      positive' = calcDistribution (student 10) positive <$> alphabets

student n x = gamma ((n + 1) / 2) / (sqrt (pi * n) * gamma (n / 2)) * (1 + x**2 / n)**(-(n +1)/2)
    where gamma = exp . logGamma

calcDistribution :: Fractional a => (a -> a) -> Vector a -> Int -> Vector a
calcDistribution f xs m = V.generate m $ \i -> len * simpson (min + step*fromIntegral i) (min + step*(fromIntegral i + 1))
    where
      len = fromIntegral $ V.length xs
      mean = V.sum xs / len
      dispersion = V.sum (fmap (\x -> (x - mean)^2) xs) / len
      min = V.head xs
      max = V.last xs
      step = (max - min) / len
      simpson a b = (sEven + sOdd) * h / 3
          where
            n = 100
            h = (b - a) / fromIntegral n
            sEven = sum [4 * f (a + fromIntegral i * h) | i <- [1, 3 .. n]]
            sOdd  = sum [2 * f (a + fromIntegral i * h) | i <- [2, 4 .. n]]

diff :: Num a => Vector a -> Vector a
diff xs = V.zipWith (-) xs (V.tail xs)

sort :: Ord a => Vector a -> Vector a
sort xs = runST $ do
            xs' <- V.thaw xs
            V.sort xs'
            V.unsafeFreeze xs'
