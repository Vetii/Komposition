module Filters where

import Komposition

type Filter a = Komposition a -> Komposition a

not :: Filter Bool
not = fmap Prelude.not

invert :: Filter Frac
invert = fmap (1-) 
