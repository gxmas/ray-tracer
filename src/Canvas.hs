{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Canvas
    ( Canvas
    , newCanvas
    , blackPixel
    , redPixel
    , greenPixel
    , bluePixel
    , whitePixel
    , readPixel
    , writePixel
    , unsafeReadPixel
    , unsafeWritePixel
    ) where

import qualified Data.Vector.Unboxed as U

-- |A type synonym to represent a @Pixel@
type Pixel  = (Double,Double,Double)

type Width  = Int
type Height = Int

-- |A @Canvas@ represents an immutable @Canvas@
data Canvas = Canvas
    { width  :: !Width
    , height :: !Height
    , vector :: !(U.Vector Pixel)
    }

-- |A @Canvas@ represents a mutable @Canvas@
data MCanvas s = MCanvas
    { width  :: !Width
    , height :: !Height
    , vector :: !(U.MVector s Pixel)
    }

type Pos  = Int
type XPos = Int
type YPos = Int

class CanvasConv c where
    fromPos :: c -> XPos -> YPos -> Pos

instance CanvasConv Canvas where
    fromPos Canvas{..} x y = y * width + x

-- |Black @Pixel@
blackPixel :: Pixel
blackPixel = (0.0, 0.0, 0.0)

-- |Red @Pixel@
redPixel :: Pixel
redPixel = (1.0, 0.0, 0.0)

-- |Green @Pixel@
greenPixel :: Pixel
greenPixel = (0.0, 1.0, 0.0)

-- |Blue @Pixel@
bluePixel :: Pixel
bluePixel = (0.0, 0.0, 1.0)

-- |White @Pixel@
whitePixel :: Pixel
whitePixel = (1.0, 1.0, 1.0)

-- |Create a new black @Canvas@ with the given @Width@ and @Height@
newCanvas :: Width -> Height -> Canvas
newCanvas w h = Canvas w h (U.replicate (w*h) blackPixel)

readPixel :: Canvas -> XPos -> YPos -> Pixel
readPixel Canvas{..} x y = vector U.! (y * width + x)

writePixel :: Canvas -> Pixel -> XPos -> YPos -> Canvas
writePixel = undefined

unsafeReadPixel :: MCanvas s -> XPos -> YPos -> MCanvas s
unsafeReadPixel = undefined

unsafeWritePixel :: MCanvas s -> Pixel -> XPos -> YPos -> MCanvas s
unsafeWritePixel = undefined