module Random where

import System.Random

getRandomElem :: (RandomGen g) => g -> [a] -> (a, g)
getRandomElem g l = let (i, g') = next g
                    in ((cycle l) !! (i `mod` (length l)), g')

getRandomElems :: (RandomGen g) => g -> [a] -> [a]
getRandomElems g l = let coins = randoms g :: [Bool]
    in map snd $ filter fst $ zip coins (cycle l)
