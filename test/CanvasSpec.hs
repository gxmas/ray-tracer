{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module CanvasSpec where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Monadic

import Canvas
import Internal.Types
import Util()

import qualified Data.Vector.Unboxed as U
import qualified Data.Vector.Unboxed.Mutable as M

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
    describe "Canvas Operation" $ do
        it "Creating new Canvas" $ property $
             \(Positive w) (Positive h) ->
                 w * h <= 5000 ==>
                     let (Canvas{..}) = newCanvas w h
                     in    w == width 
                        && h == height 
                        && U.length vdata == w*h 
                        && U.all (== blackPixel) vdata
        it "Creating new MCanvas" $ property $
            \(Positive w) (Positive h) ->
                w * h <= 5000 ==> monadicST $ do
                    MCanvas{..} <- run $ newMCanvas w h
                    assert $    w == m'width
                             && h == m'height
                             && M.length m'vdata == w*h
                             -- && M.all (== blackPixel) m'vdata
        it "Read Pixel from new immutable Canvas" $ property $
            \c -> do
                x <- choose (0, width c -1)
                y <- choose (0, height c -1)
                return $ readPixel c x y === blackPixel
        it "Read Pixel from new mutable Canvas" $ property $
            \(Positive w) (Positive h) -> monadicST $ do
                p <- run $ do
                    mc <- newMCanvas w h
                    m'readPixel mc 0 0 
                assert $ p == blackPixel
        it "Write Pixel to new mutable Canvas" $ property $
            \(Positive w) (Positive h) p -> 
                w * h <= 5000 && p /= blackPixel ==> monadicST $ do
                p' <- run $ do
                    mc <- newMCanvas w h
                    m'writePixel mc p 0 0
                    m'readPixel mc 0 0 
                assert $ p == p'