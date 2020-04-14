module Types where

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