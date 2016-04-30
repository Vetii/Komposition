module Transformations.Complex where

import Data.Complex
import qualified Data.Vect.Float as V
import Komposition
import qualified Transformations.Basic as TB

type ComplexTransformation a = Complex a -> Complex a

fromComplex :: Complex Float -> V.Vec2 
fromComplex z = V.Vec2 (realPart z) (imagPart z)

toComplex :: V.Vec2 -> Complex Float
toComplex (V.Vec2 x y) = x :+ y

transform :: ComplexTransformation Float
  -> Komposition b -> Komposition b
transform t = TB.transform (fromComplex . t . toComplex)

-- Riemann Sphere
data RiemannComplex a = RC (Complex a) | Infinity
    deriving (Show)

(+:) :: a -> a -> RiemannComplex a
(+:) x y = RC (x :+ y)

instance (RealFloat a, Num a) => Num (RiemannComplex a) where
    (+) z Infinity   = Infinity
    (+) Infinity z   = Infinity
    (RC z) + (RC z') = RC (z + z')
    (*) z Infinity   = Infinity
    (*) Infinity z   = Infinity
    (RC z) * (RC z') = RC (z * z')
    abs Infinity = Infinity
    abs (RC z)   = RC (abs z)
    signum Infinity = 1
    signum (RC z)  = RC (signum z)
    fromInteger = RC . fromInteger
    negate Infinity = Infinity
    negate (RC z)   = RC (negate z)
    (-) z Infinity = Infinity
    (-) Infinity z = Infinity
    (-) (RC z) (RC z') = RC (z - z')

instance (RealFloat a) => Fractional (RiemannComplex a) where
    (/) z (RC 0) = Infinity
    (/) z Infinity = RC 0
    (RC z) / (RC z') = RC (z / z')
    fromRational a = RC (fromRational a) 

-- Möbius transforms
data Mobius t = Mobius { a :: Complex t,
                         b :: Complex t,
                         c :: Complex t,
                         d :: Complex t }
  deriving (Show, Eq)

-- Mobius Transformations form a group under composition
instance (RealFloat a) => Monoid (Mobius a) where
  mappend a b = compose b a
  mempty = Mobius 1 0 0 1

compose :: (RealFloat a) => Mobius a -> Mobius a -> Mobius a
compose (Mobius a b c d)
        (Mobius a' b' c' d') = let a2 = a * a' + b * c'
                                   b2 = a * b' + b * d'
                                   c2 = c * a' + d * c'
                                   d2 = c * b' + d * d'
        in Mobius a2 b2 c2 d2

-- Monoid action on a complex
act :: (RealFloat a) => Mobius a -> ComplexTransformation a
act (Mobius a b c d) z = (a * z + b) / (c * z + d)

-- Determinant
det :: (RealFloat a) => Mobius a -> Complex a
det (Mobius a b c d) = a * d - b * c

-- Trace
trace :: (RealFloat a) => Mobius a -> Complex a
trace m = a m + d m

-- Build a Mobius transform with determinant = 1
normalize :: (RealFloat a) => Mobius a -> Mobius a
normalize (Mobius a b c d) = Mobius a' b' c' d'
  where div = sqrt $ det (Mobius a b c d)
        a' = a / div
        b' = b / div
        c' = c / div
        d' = d / div

mkMobius :: (RealFloat a) => 
  Complex a ->
  Complex a ->
  Complex a ->
  Complex a -> Mobius a
mkMobius a b c d = normalize $ Mobius a b c d

-- Inverse a Mobius transform
inverse :: (RealFloat a) => Mobius a -> Mobius a
inverse (Mobius a b c d) = Mobius d (-b) (-c) a

power :: (RealFloat a) => Mobius a -> Int -> Mobius a
power m n
  | n < 0     = mconcat $ replicate (abs n) (inverse m)
  | otherwise = mconcat $ replicate n m

-- RECIPES
-- Normal transformations
translate :: (RealFloat a) => Complex a -> Mobius a
translate b = Mobius 1 b 0 1

multiply :: (RealFloat a) => Complex a -> Mobius a
multiply k = Mobius (sqrt k) 0 0 (1 / sqrt k)

scale :: (RealFloat a) => a -> Mobius a
scale k = multiply (k :+ 0)

rotate :: (RealFloat a) => a -> Mobius a
rotate theta = multiply $ mkPolar 1 theta

-- Switch poles of the Riemann sphere (Normalized form)
switchPoles :: (RealFloat a) => Mobius a
switchPoles = mkMobius 0 (0 :+ 1) (0 :+ 1) 0

class SpecialMobius sm where
  getMobius :: sm b -> Mobius b

-- RECIPE 1: Maps which carry the real axis to itself
newtype UpperHalfPlaneMobius a = UpperHalfPlaneMobius (Mobius a)
instance SpecialMobius UpperHalfPlaneMobius where
  getMobius (UpperHalfPlaneMobius m) = m

mkUHP :: (RealFloat a) => a -> a -> a -> a -> UpperHalfPlaneMobius a
mkUHP a b c d = UpperHalfPlaneMobius $ mkMobius (a :+ 0) (b :+ 0) (c :+ 0) (d :+ 0)

-- RECIPE 2: Maps which carry the real axis to the unit circle
newtype CayleyMap a = CayleyMap (Mobius a)
instance SpecialMobius CayleyMap where
  getMobius (CayleyMap m) = m

mkCayley :: (RealFloat a) => CayleyMap a
mkCayley = CayleyMap $ mkMobius 1 (0 :+ (-1)) 1 (0 :+ 1)

-- RECIPE 3: Maps which carry the unit circle to itself
newtype UnitCircleMobius a = UnitCircleMobius (Mobius a)
instance SpecialMobius UnitCircleMobius where
  getMobius (UnitCircleMobius m) = m

mkUC :: (RealFloat a) => UpperHalfPlaneMobius a -> UnitCircleMobius a
mkUC (UpperHalfPlaneMobius m) = UnitCircleMobius $
  mconcat [inverse (getMobius mkCayley), m, getMobius mkCayley]

-- RECIPE 4: Maps which pair circles
data Circle a = Circle { center :: Complex a, r :: a}
  deriving (Show)

newtype PairCircleMobius a = PairCircleMobius (Mobius a)
instance SpecialMobius PairCircleMobius where
  getMobius (PairCircleMobius m) = m

mkPCircles :: (RealFloat a) => 
  Circle a ->
  Circle a ->
  UnitCircleMobius a ->
  PairCircleMobius a
mkPCircles (Circle p r) (Circle q s) (UnitCircleMobius m) = 
  let centerZero = translate (-p) -- C1: Center C on zero
      toUnit     = scale (1/r) -- C1' : scales C1 to the unit circle
      invert     = switchPoles -- C1'': invert of C1
      scaleS     = scale s -- C2 : C1'' with radius s
      centerQ    = translate q -- C' = C2 translated on q
  in PairCircleMobius $ normalize 
                      $ mconcat [centerZero, toUnit, m, invert, scaleS, centerQ]

mobiusCircle :: (RealFloat a) => Mobius a -> Circle a -> Circle a
mobiusCircle (Mobius a b c d) (Circle p r) = 
  let z = p - ((r^2 :+ 0) / conjugate (d / c + p))
      q = act (Mobius a b c d) z
      s = magnitude $ q - act (Mobius a b c d) (p + (r :+ 0))
  in Circle q s

test :: (RealFloat a) => (Circle a, Circle a)
test = let c1 = Circle ((-2) :+ 1) 1
           c2 = Circle (2 :+ 1) 2
           m  = mkPCircles c1 c2 $ mkUC (mkUHP 1 2 3 4)
       in (mobiusCircle (getMobius m) c1, mobiusCircle (inverse (getMobius m)) c2)
