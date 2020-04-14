module PointSpec where

import Test.Hspec
import Test.QuickCheck

import Point
import Types
import Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Point Operations" $ do
        it "Add a Vector to a Point" $ property $
            \p1 p2 p3 v1 v2 v3 ->
                let result   = (Point p1 p2 p3) `addVector` (Vector v1 v2 v3)
                    expected = Point (p1+v1) (p2+v2) (p3+v3)
                in expected ~== result
        it "Substracting a Point to a Point" $ property $
            \x1 y1 z1 x2 y2 z2 ->
                let result   = (Point x1 y1 z1) `subPoint` (Point x2 y2 z2)
                    expected = Point (x1-x2) (y1-y2) (z1-z2)
                in expected ~== result
        it "Substracting a Vector to a Point" $ property $
            \p1 p2 p3 v1 v2 v3 ->
                let result   = (Point p1 p2 p3) `subVector` (Vector v1 v2 v3)
                    expected = Point (p1-v1) (p2-v2) (p3-v3)
                in expected ~== result