module Transformations.Basic where

import Komposition
import Data.Vect.Float
import Data.Vect.Float.Util.Dim2 (angle2, rotMatrix2)

type Transformation = Vec2 -> Vec2

-- Apply a transformation, work like they do in Processing
transform :: Transformation -> Komposition a -> Komposition a
transform t (Komposition f) = Komposition (f . t)

translate :: Vec2 -> Transformation
translate d = (d &+)

relativeTo :: Point -> Transformation 
relativeTo p = (p &-)

scale :: Float -> Transformation
scale = scalarMul

rotate :: Float -> Transformation
rotate = rotate2

-- Polar / Cartesian
fromPolar :: PolarPoint -> Vec2
fromPolar (Vec2 r t) = Vec2 (r * cos t) (r * sin t)

toPolar :: Vec2 -> PolarPoint
toPolar p = Vec2 (len p) (angle2 p)

center :: Float -> Float -> Transformation 
center w h = translate $ Vec2 (-w/2) (-h/2)

flipX :: Transformation 
flipX (Vec2 x y) = Vec2 (-x) y

flipY :: Transformation 
flipY (Vec2 x y) = Vec2 x (-y)

circleInversion :: Vec2 -> Float -> Transformation
circleInversion x0 k x = x0 &+ scalarMul (1/(len (x &- x0) ^ 2)) (scalarMul (k ^ 2) (x &- x0))

swirl :: Float -> Transformation
swirl f p = rotate (f * log (len p)) p
