module Grid where

import Control.Monad

-- Interval has two bounds
-- data Interval a = Interval { min :: a, max :: a }
type Interval a = (a, a)

-- Interval for each dimension, creates a n-dimensional cube.
type Domain a = [Interval a]

-- A cutter cuts an interval in many
type Cutter a = Interval a -> [Interval a]

-- How to decide if I want to cut a domain or not
type CutCondition a = Domain a -> Bool

-- A grid is a number of non overlapping domains
type Grid a = [Domain a]

-- Creates a grid from a domain
cutDomain :: Cutter a -> Domain a -> Grid a 
cutDomain cut dom = sequence $ map cut dom
