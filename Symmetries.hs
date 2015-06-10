module Symmetries where

import Komposition
import Transformations
import Blendings

-- Symmetries return the image of a point, 
-- the blending is left to the user
-- the blending should be commutative
symmetrize :: Transformation -> -- Transformation (symmetry)
  Blending a -> -- Blend mode
  Image a -> -- Base image
  Image a -- Result
symmetrize s b i = b (transform s i) i

centralSymmetry :: Point -> Transformation
centralSymmetry (cx,cy) (x,y) = (x',y')
  where x' = cx - (x - cx)
        y' = cy - (y - cy)

dummy :: Point -> Transformation
dummy (cx, cy) (x,y) = (x', y')
  where dx = x - cx
        dy = y - cy
        x' = cx - abs dx
        y' = cy - abs dy
