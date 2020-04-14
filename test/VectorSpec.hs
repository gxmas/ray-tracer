module VectorSpec where

import Test.Hspec
import Test.QuickCheck

import Types
import Util
import Vector

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Vector Operations" $ do
        it "Adding a Vector to a Vector" $ property $
            \x1 y1 z1 x2 y2 z2 ->
                let result   = (Vector x1 y1 z1) `addVector` (Vector x2 y2 z2)
                    expected = Vector (x1+x2) (y1+y2) (z1+z2)
                in expected ~== result
        it "Substracting a Vector to a Vector" $ property $
            \x1 y1 z1 x2 y2 z2 ->
                let result   = (Vector x1 y1 z1) `subVector` (Vector x2 y2 z2)
                    expected = Vector (x1-x2) (y1-y2) (z1-z2)
                in expected ~== result
        it "Negating a Vector" $ property $
            \x y z ->
                let result   = negVector (Vector x y z)
                    expected = Vector (-x) (-y) (-z)
                in expected ~== result
        it "Multiplicating a Vector by a Scalar" $ property $
            \x y z s ->
                let result   = (Vector x y z) `scalarMul` s
                    expected = Vector (s*x) (s*y) (s*z)
                in expected ~== result
        it "Dividing a Vector by a Scalar" $ property $
            \x y z s -> s /= 0.0 ==>
                let result   = (Vector x y z) `scalarDiv` s
                    expected = Vector (x/s) (y/s) (z/s)
                in expected ~== result
        it "Magnitude of a Vector" $ property $
            \x y z ->
                let result   = magnitude (Vector x y z)
                    expected = sqrt $ x**2 + y**2 + z**2
                in expected ~== result
        it "Normalize a Vector" $ property $
            \x y z -> magnitude (Vector x y z) /= 0.0 ==>
                let result   = normalize (Vector x y z)
                    expected = Vector (x/m) (y/m) (z/m)
                    m        = magnitude (Vector x y z)
                in expected ~== result
        it "Compute the Dot-Product of two Vectors" $ property $
            \x1 y1 z1 x2 y2 z2 ->
                let result   = (Vector x1 y1 z1) `dotProduct` (Vector x2 y2 z2)
                    expected = x1*x2 + y1*y2 + z1*z2
                in expected ~== result
        it "Compute the Cross-Product of two Vectors" $ property $
            \x1 y1 z1 x2 y2 z2 ->
                let result   = (Vector x1 y1 z1) `crossProduct` (Vector x2 y2 z2)
                    expected = Vector (y1*z2-y2*z1) (x2*z1-x1*z2) (x1*y2-x2*y1)
                in expected ~== result
    describe "Vector Properties" $ do
        it "Zero is the identity under addition" $ property $
            \v -> conjoin [ zero `addVector` v ~== v `addVector` zero
                          , zero `addVector` v ~== v
                          , v `addVector` zero ~== v
                          ]
        it "Vector Addition is Commutative" $ property $
            \v1 v2 -> v1 `addVector` v2 ~== v2 `addVector` v1
        it "Vector Addition is Associative" $ property $
            \v1 v2 v3 -> (v1 `addVector` v2) `addVector` v3 ~== v1 `addVector` (v2 `addVector` v3)
        it "Negating a Vector is Involutive" $ property $
            \v -> (negVector . negVector) v ~== v
        it "Normalized Vector has Magnitue of 1.0" $ property $
            \v -> v /= zero ==>
                magnitude (normalize v) ~== 1.0
        it "Dot Product of a unit Vector with itself is 1.0" $ property $
            \v -> v /= zero ==>
                let v' = normalize v
                in v' `dotProduct` v' ~== 1.0
        it "Dot Product of two unit Vectors of opposite direction is -1.0" $ property $
            \v -> v /= zero ==>
                let v' = normalize v
                in v' `dotProduct` (negVector v') ~== (-1.0)
        it "Cross w v = negate (Cross v w)" $ property $
            \ v w -> crossProduct w v ~== negVector (crossProduct v w)