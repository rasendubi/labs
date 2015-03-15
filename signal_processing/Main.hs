import Data.Complex

import Data.Function
import Data.Vector (Vector)
import qualified Data.Vector as V

import Graphics.EasyPlot

import FFT (fft, dft)

data Polar a = Polar { p :: a, q :: a } deriving (Read, Show, Eq)

i :: Num a => Complex a
i = 0 :+ 1

forEachIndex :: (Num i, Enum i) => [a] -> (i -> a -> b) -> [b]
forEachIndex xs f = zipWith f [0..] xs

defuzz :: (RealFloat a) => [Complex a] -> [Complex a]
defuzz = map (\(r :+ i) -> df r :+ df i)
    where df x = if abs x < 1.0e-6 then 0 else x

discrete :: (Num a, Enum a) => a -> a -> a -> (a -> b) -> Vector b
discrete from to step f = V.fromList $ map f [from, from+step .. to-step]

-- discrete' = discrete 0 (12*pi) (pi/100)
discrete' = discrete 0 (2*pi) (pi/32)

enumerate :: Vector a -> [(Double, a)]
enumerate = V.toList . V.imap (\i x -> (fromIntegral i , x))

myFunc :: Double -> Double
myFunc = sin
-- myFunc x = 2*sin x + cos (x*6) + 0.5*sin (x/2) - 0.2*sin (x*12)

-- | Translates complex number to magnitude and phase
toPolar :: RealFloat a => Complex a -> Polar a
toPolar x = Polar (magnitude x) (phase x)

-- main :: IO ()
main = do
    let input = fmap (:+ 0) $ discrete' myFunc
    let reference = dft input
    let output = fft input
    plot X11 [ Data2D [Title "Input"] [] (enumerate (V.map realPart input)) ]
    plot X11
        [ Data2D [Title "FFT"] [] (enumerate (V.map magnitude output))
        , Data2D [Title "DFT"] [] (enumerate (V.map magnitude reference)) ]
    plot X11
        [ Data2D [Title "FFT Phase"] [] (enumerate (V.map phase output))
        , Data2D [Title "DFT Phase"] [] (enumerate (V.map phase reference)) ]
    let difference = V.zipWith ((abs .) . (-) `on` magnitude) output reference
    plot X11 [ Data2D [Title "Difference"] [] (enumerate difference) ]

