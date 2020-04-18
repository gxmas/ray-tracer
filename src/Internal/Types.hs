{-# LANGUAGE DuplicateRecordFields #-}

module Internal.Types where

import qualified Data.Vector.Unboxed as U

data Point = Point
    { px :: !Double
    , py :: !Double
    , pz :: !Double
    } deriving (Eq, Show)

data Vector = Vector
    { vx :: !Double
    , vy :: !Double
    , vz :: !Double
    } deriving (Eq, Show)

data Color = Color
    { red   :: !Double
    , green :: !Double
    , blue  :: !Double
    } deriving (Eq, Show)


-- |A type synonym to represent a @Pixel@
type Pixel  = (Double,Double,Double)

type Width  = Int
type Height = Int

-- |A @Canvas@ represents an immutable @Canvas@
data Canvas = Canvas
    { width  :: !Width
    , height :: !Height
    , vdata :: !(U.Vector Pixel)
    }

-- |A @Canvas@ represents a mutable @Canvas@
data MCanvas s = MCanvas
    { width  :: !Width
    , height :: !Height
    , vdata :: !(U.MVector s Pixel)
    }

type Pos  = Int
type XPos = Int
type YPos = Int

class CanvasConv c where
    fromPos :: c -> XPos -> YPos -> Pos

instance CanvasConv Canvas where
    fromPos (Canvas w _ _) x y = y * w + x

instance CanvasConv (MCanvas s) where
    fromPos (MCanvas w _ _) x y = y * w + x