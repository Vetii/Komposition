module Komposition where

import Control.Applicative
import Control.Monad
import Data.Function

type Point = (Float, Float)
type PolarPoint = (Float, Float)

--type Image a = (Point -> a)
data Image a = Image (Point -> a)

instance Functor Image where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Image i) = Image (f . i)

instance Applicative Image where
    -- pure :: a -> f a
    pure i = Image (pure i)
    -- (<*>) :: f (a -> b) -> f a -> f b
    (<*>) (Image f) (Image i) = Image (f <*> i)

instance Monad Image where
    return = pure
    -- (>>=) :: m a -> (a -> m b) -> m b
    (>>=) i f = paste $ fmap f i

paste :: Image (Image a) -> Image a
paste (Image s) = Image $ fetch <*> s

type Frac = Float

fetch :: Point -> Image a -> a
fetch p (Image i) = i p

mag :: Point -> Float 
mag (x,y) = sqrt (x^2 + y^2)

constraint :: (Ord a, Num a) => a -> a -> a -> a
constraint mini maxi x = min (max x mini) maxi

fromBool :: Bool -> Frac
fromBool True = 1.0
fromBool False = 0.0

toIntegral :: (Integral a) => Frac -> a
toIntegral x = floor $ 255 * constraint 0 1 x
