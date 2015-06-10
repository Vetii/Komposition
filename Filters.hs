module Filters where

import Komposition

type Filter a = Image a -> Image a

not :: Filter Bool
not = fmap (Prelude.not) 

invert :: Filter Frac
invert = fmap (1-) 
