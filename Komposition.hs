module Komposition where

import Control.Applicative
import Control.Monad
import Data.Function
import Data.Vect.Float

type Point = Vec2
type PolarPoint = Vec2

--type Komposition a = (Point -> a)
data Komposition a = Komposition (Point -> a)

instance Functor Komposition where
    -- fmap :: (a -> b) -> f a -> f b
    fmap f (Komposition i) = Komposition (f . i)

instance Applicative Komposition where
    -- pure :: a -> f a
    pure i = Komposition (pure i)
    -- (<*>) :: f (a -> b) -> f a -> f b
    (<*>) (Komposition f) (Komposition i) = Komposition (f <*> i)

instance Monad Komposition where
    return = pure
    -- (>>=) :: m a -> (a -> m b) -> m b
    (>>=) i f = paste $ fmap f i

paste :: Komposition (Komposition a) -> Komposition a
paste (Komposition s) = Komposition $ fetch <*> s

type Frac = Float

fetch :: Point -> Komposition a -> a
fetch p (Komposition i) = i p

clamp :: (Ord a, Num a) => a -> a -> a -> a
clamp mini maxi x = min (max x mini) maxi

smoothstep :: (Ord a, Num a) => a -> a
smoothstep x = 3 * x'^2 - 2 * x'^3
  where x' = clamp 0 1 x

fromBool :: Bool -> Frac
fromBool True = 1.0
fromBool False = 0.0

toIntegral :: (Integral a) => Frac -> a
toIntegral x = floor $ 255 * clamp 0 1 x
