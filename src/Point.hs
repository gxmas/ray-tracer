{-# LANGUAGE RecordWildCards #-}

module Point
    ( Point(..)
    , addVector
    , subPoint
    , subVector
    ) where

import Internal.Types

-- |Add a @Vector@ to a @Point@ by adding the components pairwise
addVector :: Point -> Vector -> Point
addVector Point{..} Vector{..} = Point (px+vx) (py+vy) (pz+vz)

-- |Subtract a @Point@ to a @Point@ by substracting the component pairwise
subPoint :: Point -> Point -> Vector
subPoint (Point x1 y1 z1) (Point x2 y2 z2) = Vector (x1-x2) (y1-y2) (z1-z2)

-- |Substract a @Vector@ from a @Point@ by substraction the components pairwise
subVector :: Point -> Vector -> Point
subVector Point{..} Vector{..} = Point (px-vx) (py-vy) (pz-vz)