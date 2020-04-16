module PointSpec where

import Test.Hspec
import Test.QuickCheck

import Point
import Vector (negVector)
import Types
import Util

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Point Operations" $ do
        it "Add a Vector to a Point" $ property $
            \p@(Point p1 p2 p3) v@(Vector v1 v2 v3) ->
                Point (p1+v1) (p2+v2) (p3+v3) ~== p `addVector` v
        it "Substracting a Point to a Point" $ property $
            \p1@(Point x1 y1 z1) p2@(Point x2 y2 z2) ->
                Vector (x1-x2) (y1-y2) (z1-z2) ~== p1 `subPoint` p2
        it "Substracting a Vector to a Point" $ property $
            \p@(Point p1 p2 p3) v@(Vector v1 v2 v3) ->
                Point (p1-v1) (p2-v2) (p3-v3) ~== p `subVector` v
    describe "Point Properties" $ do
        it "Point1 - Point2 = negate (Point 2 - Point1)" $ property $
            \p1 p2 -> p1 `subPoint` p2 ~== negVector (p2 `subPoint` p1)
