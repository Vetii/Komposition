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
translational v p = let 
    nrml = project p v     -- get projection on normal of v
    proj = p &- nrml -- get projection along v
    dir  = normalize v 
    l = len v
    trgtLength = triangle l l (len proj)
    in scalarMul trgtLength dir &+ nrml

axial :: Vec2 -> Transformation
axial v = chBase newbase . axis . chBase (inverse newbase)
    where nv = normalize v
          normal = rotate (pi / 2) nv
          newbase = (Mat2 nv normal)

