{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Canvas
    ( Canvas
    , Width
    , Height
    , Pixel
    , XPos
    , YPos
    , newCanvas
    , newMCanvas
    , withCanvas
    , ppmCanvas
    , thawCanvas
    , freezeCanvas
    , blackPixel
    , redPixel
    , greenPixel
    , bluePixel
    , whitePixel
    , readPixel
    , m'readPixel
    , m'writePixel
    ) where

import           Control.Monad.Primitive
import           Data.ByteString.Lazy (ByteString)
import           Data.ByteString.Builder
import qualified Data.Vector.Unboxed         as U
import qualified Data.Vector.Unboxed.Mutable as M
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

-- |Create a new black immutable @Canvas@ with the given @Width@ and @Height@
newCanvas :: Width -> Height -> Canvas
newCanvas w h = Canvas w h (U.replicate (w*h) blackPixel)

-- |Create a new black mutable @MCanvas@ with the given @Width@ and @Height@
newMCanvas :: PrimMonad m => Width -> Height -> m (MCanvas (PrimState m))
newMCanvas w h = MCanvas w h <$> M.replicate (w*h) blackPixel

-- |Thaw an immutable @Canvas@ to a mutale @MCanvas@
thawCanvas :: PrimMonad m => Canvas -> m (MCanvas (PrimState m))
thawCanvas (Canvas w h v) = U.thaw v >>=  pure . MCanvas w h

-- |Freeze a mutable @Canvas@ to an immutable @MCanvas@
freezeCanvas :: PrimMonad m => MCanvas (PrimState m) -> m Canvas
freezeCanvas (MCanvas w h mv) = U.freeze mv >>= pure . Canvas w h

withCanvas :: PrimMonad m
           => Canvas
           -> (MCanvas (PrimState m) -> m (MCanvas (PrimState m)))
           -> m Canvas
withCanvas c go = thawCanvas c >>= go >>= freezeCanvas

-- |Read a @Pixel@ from an immutable @Canvas@ wihtout bounds checking
readPixel :: Canvas -> XPos -> YPos -> Pixel
readPixel c x y = vdata c U.! fromPos c x y

-- |Read a @Pixel@ from a mutable @Canvas@ without bounds checking
m'readPixel :: PrimMonad m => MCanvas (PrimState m) -> XPos -> YPos -> m Pixel
m'readPixel mc x y = M.read (m'vdata mc) (fromPos mc x y)

-- |Write a @Pixel@ to a mutable @Canvas@ without bounds checking
m'writePixel :: PrimMonad m => MCanvas (PrimState m) -> Pixel -> XPos -> YPos -> m ()
m'writePixel mc p x y = M.write (m'vdata mc) (fromPos mc x y) p

-- |Write a @Canvas@ as a PPM image (P3)
ppmCanvas :: Canvas -> Int -> ByteString
ppmCanvas Canvas{..} s = toLazyByteString $
         ppmHeader width height s
      <> ppmImage s vdata
      <> nl

-- Generate a PPM header section for a given @Width@ and @Height@ and scaling factor
ppmHeader :: Width -> Height -> Int -> Builder
ppmHeader w h s = byteString "P3"            <> nl
               <> intDec w <> sp <> intDec h <> nl
               <> intDec s                   <> nl

ppmImage :: Int -> U.Vector Pixel -> Builder
ppmImage s = U.foldl' (\b p -> b <> pixel p <> nl) mempty
    where pixel = ppmPixel (fromIntegral s)

ppmPixel ::  Double -> Pixel -> Builder
ppmPixel s (r,g,b) = intDec r' <> sp <> intDec g' <> sp <> intDec b'
    where (r',g',b') = (clamp r, clamp g, clamp b)
          clamp      = floor . (max 0.0 . min s) . (s*)

sp :: Builder
sp = char8 ' '

nl :: Builder
nl = char8 '\n'
