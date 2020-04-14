module Point where

data Point = Point
    { px :: !Double
    , py :: !Double
    , pz :: !Double
    } deriving (Eq, Show)
