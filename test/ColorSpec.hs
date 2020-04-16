module ColorSpec where

import Test.Hspec
import Test.QuickCheck

import Color
import Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Color Operations" $ do
        it "Adding a Color to a Color" $ property $
            \c1@(Color r1 g1 b1) c2@(Color r2 g2 b2) ->
                Color (r1+r2) (g1+g2) (b1+b2) ~== c1 `addColor` c2
        it "Substracting a Color to a Color" $ property $
            \c1@(Color r1 g1 b1) c2@(Color r2 g2 b2) ->
                Color (r1-r2) (g1-g2) (b1-b2) ~== c1 `subColor` c2
        it "Multiplicating a Color by a Scalar" $ property $
            \c@(Color r g b) s ->
                Color (s*r) (s*g) (s*b) ~== c `scalarMul` s
        it "Multiplying a Color to a Color" $ property $
            \c1@(Color r1 g1 b1) c2@(Color r2 g2 b2) ->
                Color (r1*r2) (g1*g2) (b1*b2) ~== c1 `mulColor` c2
    describe "Color Properties" $ do
        it "Color Addition is Associative" $ property $
            \c1 c2 c3 ->
                (c1 `addColor` c2) `addColor` c3 ~== c1 `addColor` (c2 `addColor` c3)
        it "Color Addition is Commutative" $ property $
            \c1 c2 ->
                c1 `addColor` c2 ~== c2 `addColor` c1
        it "Addition and Subtraction are reversible operations" $ property $
            \c1 c2 ->
                c1 ~== c1 `addColor` c2 `subColor` c2
        it "Subtraction and Addition are reversible operations" $ property $
            \c1 c2 ->
                c1 ~== c1 `subColor` c2 `addColor` c2
        it "Equivalence between Color addition and scalar mulplication" $ property $
            \c ->
                c `addColor` c ~== c `scalarMul` 2.0
        it "Hadamar Product is Color Multiplication" $ property $
            \c1 c2 ->
                c1 `mulColor` c2 ~== c1 `hadamar` c2