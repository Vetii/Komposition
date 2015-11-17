module Symmetries where

import Komposition
import Transformations.Basic
import Blendings

import Data.Fixed
import Data.Vect.Float
import Data.Vect.Float.Util.Dim2 (angle2)


-- Symmetries return the image of a point, 
-- the blending is left to the user
-- the blending should be commutative
symmetrize :: Transformation -> -- Transformation (symmetry)
  Blending a -> -- Blend mode
  Komposition a -> -- Base image
  Komposition a -- Result
symmetrize s b i = b (transform s i) i

triangle :: (Floating a, Fractional a) => a -> a -> a -> a
triangle a p x = (a/pi) * acos (cos (pi * x / p))

rotational :: (Integral a) => a -> Transformation
rotational o p  = fromPolar (Vec2 r t)
  where r = len p
        t = triangle ph ph $ angle2 p
        ph = pi / (fromIntegral o)

repeat :: Float -> Transformation
repeat d (Vec2 x y) = Vec2 (triangle d d x) y

axis :: Transformation
axis (Vec2 x y) = Vec2 x (abs y)

translational :: Vec2 -> Transformation
translational v = Symmetries.repeat mag . rotate phase
  where (mag, phase) = (len v, angle2 v)

axial :: Vec2 -> Transformation
axial v = axis . rotate phase 
  where (mag, phase) = (len v, angle2 v)
