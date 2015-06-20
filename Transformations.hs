module Transformations where

import Komposition

type Transformation = Point -> Point
type Vector = (Float, Float)

-- Apply a transformation, work like they do in Processing
transform :: Transformation -> Image a -> Image a
transform t (Image f) = Image (f . t)

translate :: Vector -> Transformation
translate (dx, dy) (x,y) = (x + dx, y + dy)

scale :: Vector -> Transformation
scale (sx, sy) (x, y) = (sx * x, sy * y)

pixelize :: Transformation
pixelize (x,y) = (fromIntegral (round x), fromIntegral (round y))

uniScale :: Float -> Transformation
uniScale s = scale (s, s)

rotate :: Float -> Transformation
rotate t (x, y) = (x * cos t - y * sin t, y * cos t + x * sin t)

-- Polar / Cartesian
fromPolar :: PolarPoint -> Point
fromPolar (r,t) = (r * cos t, r * sin t)

toPolar   :: Point -> PolarPoint
toPolar   (x,y) = (mag (x, y), atan2 y x) 

center :: Vector -> Transformation 
center (w,h) = translate (-w/2, -h/2)

flipX :: Transformation 
flipX (x,y) = (-x,y)

flipY :: Transformation 
flipY (x,y) = (x,-y)
