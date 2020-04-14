module Util where

import Test.QuickCheck.Arbitrary
import Types

class FloatEq a where
    infix 4 ~==
    (~==) :: a -> a -> Bool

instance FloatEq Double where
    l ~== r = abs(l - r) < epsilon
        where epsilon = 0.00000001

instance FloatEq Point where
    (Point x1 y1 z1) ~== (Point x2 y2 z2) = x1 ~== x2 && y1 ~== y2 && z1 ~== z2

instance FloatEq Vector where
    (Vector x1 y1 z1) ~== (Vector x2 y2 z2) = x1 ~== x2 && y1 ~== y2 && z1 ~== z2


instance Arbitrary Vector where
    arbitrary = Vector <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Point where
    arbitrary = Point <$> arbitrary <*> arbitrary <*> arbitrary