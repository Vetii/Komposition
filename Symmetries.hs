module Symmetries where

import Komposition
import Transformations
import Blendings

import Data.Fixed

-- Symmetries return the image of a point, 
-- the blending is left to the user
-- the blending should be commutative
symmetrize :: Transformation -> -- Transformation (symmetry)
  Blending a -> -- Blend mode
  Image a -> -- Base image
  Image a -- Result
symmetrize s b i = b (transform s i) i

central :: Point -> Transformation
central (cx, cy) (x,y) = (x', y')
  where dx = x - cx
        dy = y - cy
        x' = cx - abs dx
        y' = cy - abs dy

rotational :: Float -> Transformation
rotational p (x,y) = fromPolar (r, t)
  where r = fst pol
        t = triangle p p $ snd pol
        pol = toPolar (x,y)

triangle a p x = (a / p) * (p - abs(x `mod'` (2*p) - p))

relativeTo :: Point -> Transformation 
relativeTo (px, py) (x,y) = (x', y')
  where x' = x - px
        y' = y - py

repeat :: Float -> Transformation
repeat d (x,y) = (triangle d d x, y)

translational :: Vector -> Transformation
translational (dx,dy) = Symmetries.repeat mag . rotate phase
  where (mag, phase) = toPolar (dx, dy)
