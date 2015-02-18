import Data.Complex

-- import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.SparkLine

import Graphics.Rendering.Chart.Easy as G
import Graphics.Rendering.Chart.Backend.Cairo as G

i :: Num a => Complex a
i = 0 :+ 1

forEachIndex :: (Num i, Enum i) => [a] -> (i -> a -> b) -> [b]
forEachIndex xs f = zipWith f [0..] xs

-- | Discrete Fourier Transform
--
--   Applies DFT to list of complex values.
dft :: (RealFloat a) => [Complex a] -> [Complex a]
dft xs = forEachIndex xs $ \k _ ->
    sum $ forEachIndex xs $ \n xn ->
        xn * exp (-i*2*pi * fromIntegral k * fromIntegral n / fromIntegral (length xs))
{-# SPECIALIZE dft :: [Complex Double] -> [Complex Double] #-}
{-# SPECIALIZE dft :: [Complex Float] -> [Complex Float] #-}

-- | Inverse Discrete Fourier Transform
--
--   idft . dft == id
idft :: (RealFloat a) => [Complex a] -> [Complex a]
idft xs = forEachIndex xs $ \k _ ->
    (1 / fromIntegral (length xs) *) $ sum $ forEachIndex xs $ \n xn ->
        xn * exp (-i*2*pi * fromIntegral k * fromIntegral n / fromIntegral (length xs))
{-# SPECIALIZE idft :: [Complex Double] -> [Complex Double] #-}
{-# SPECIALIZE idft :: [Complex Float] -> [Complex Float] #-}

defuzz :: (RealFloat a) => [Complex a] -> [Complex a]
defuzz = map (\(r :+ i) -> df r :+ df i)
    where df x = if abs x < 1.0e-6 then 0 else x

-- renderReal :: SparkOptions -> [Complex Double] -> SparkLine
-- renderReal opts = SparkLine opts . map realPart
-- 
-- renderableDft :: [Complex Double] -> Renderable ()
-- renderableDft = toRenderable . renderReal smoothSpark . dft

sinLine :: [Complex Double]
sinLine = map (\x -> sin (fromIntegral x/pi)) [0..100]

sample :: [(Double, Double)]
sample = [(1.0,1.0),(2,2),(3,1)]

enumerate :: [Double] -> [(Double, Double)]
enumerate = flip forEachIndex $ \i x -> (fromIntegral i, x)

amplitude (re :+ im) = sqrt $ re^2 + im^2

main :: IO ()
main = G.toFile def "1.png" $ do
    -- layout_title .= "Hello, world!"
    plotLeft (line "original" [enumerate $ map realPart sinLine])
    let result = dft sinLine
    plotRight (line "amplitude" [enumerate $ map amplitude result])
-- main = interact $ unlines . map (unlines . map show . dft . read) . lines
