{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Util where

import Canvas (newCanvas, newMCanvas)
import Control.Monad.ST
import Internal.Types
import Test.QuickCheck.Arbitrary
import Test.QuickCheck.Gen

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

instance FloatEq Color where
    (Color r1 g1 b1) ~== (Color r2 g2 b2) = r1 ~== r2 && g1 ~== g2 && b1 ~== b2


instance Arbitrary Vector where
    arbitrary = Vector <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Point where
    arbitrary = Point <$> arbitrary <*> arbitrary <*> arbitrary

instance Arbitrary Canvas where
    arbitrary = do
        w <- choose (1, 500)
        h <- choose (1, 500)
        pure $ newCanvas w h

instance Arbitrary (IO (MCanvas RealWorld)) where
    arbitrary = return $ newMCanvas 20 20 

instance Arbitrary (ST s (MCanvas s)) where
    arbitrary = return $ newMCanvas 20 20 

instance Arbitrary Color where
    arbitrary = Color <$> arbitrary <*> arbitrary <*> arbitrary