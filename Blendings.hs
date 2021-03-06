module Blendings where

import Komposition
import Control.Applicative

type Blending a = Komposition a -> Komposition a -> Komposition a

blendWith :: (a -> a -> a)
    -> Komposition a
    -> Komposition a
    -> Komposition a
blendWith f i1 i2 = f <$> i1 <*> i2 

instance (Num a) => Num (Komposition a) where
    (+) = blendWith (+)
    (*) = blendWith (*)
    (-) = blendWith (-)
    abs = fmap abs
    signum = fmap signum
    fromInteger x = Komposition (const (fromInteger x))

add :: (Num a) => Blending a
add = blendWith (+)

substract :: (Num a) => Blending a
substract = blendWith (-)

multiply :: (Num a) => Blending a
multiply = blendWith (*)

divide :: (Fractional a) => Blending a
divide = blendWith (/)

maxBlend :: (Ord a) => Blending a
maxBlend = blendWith max

minBlend :: (Ord a) => Blending a
minBlend = blendWith min

average :: (Num a, Fractional a) => [Komposition a] -> Komposition a
average imgs = fmap (\v -> v / fromIntegral (length imgs)) (sum imgs)

or :: Blending Bool
or = blendWith (||)

and :: Blending Bool
and = blendWith (&&)
