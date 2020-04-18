{-# LANGUAGE RecordWildCards #-}

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
import           Internal.Types

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
readPixel Canvas{..} x y = vdata U.! (y * width + x)

writePixel :: Canvas -> Pixel -> XPos -> YPos -> Canvas
writePixel = undefined

unsafeReadPixel :: MCanvas s -> XPos -> YPos -> MCanvas s
unsafeReadPixel = undefined

unsafeWritePixel :: MCanvas s -> Pixel -> XPos -> YPos -> MCanvas s
unsafeWritePixel = undefined