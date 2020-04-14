module Vector where

data Vector = Vector
    { vx :: !Double
    , vy :: !Double
    , vz :: !Double
    } deriving (Eq, Show)
