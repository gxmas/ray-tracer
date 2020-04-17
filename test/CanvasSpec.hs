{-# LANGUAGE RecordWildCards #-}

module CanvasSpec where

import Test.Hspec
import Test.QuickCheck

import Canvas

import qualified Data.Vector.Unboxed as U

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    describe "Pixel Definition" $ do
        it "Color Pixel" $ do
            blackPixel `shouldBe` (0.0, 0.0, 0.0)
            redPixel `shouldBe` (1.0, 0.0, 0.0)
            greenPixel `shouldBe` (0.0, 1.0, 0.0)
            bluePixel `shouldBe` (0.0, 0.0, 1.0)
            whitePixel `shouldBe` (1.0, 1.0, 1.0)
--    describe "Canvas Operation" $ do
--         it "Creating new Canvas" $ property $
--             \(Positive w) (Positive h) ->
--                 w * h <= 50000 ==>
--                     let c@(Canvas{..}) = newCanvas w h
--                     in U.length vector == w*h && U.all (== blackPixel) c