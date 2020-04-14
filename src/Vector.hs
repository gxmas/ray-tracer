{-# LANGUAGE RecordWildCards #-}

module Vector
    ( Vector(..)
    , zero
    , addVector
    , subVector
    , negVector
    , scalarMul
    , scalarDiv
    , magnitude
    , normalize
    , dotProduct
    , crossProduct
    ) where

import Types

-- |@zero@ is a @Vector@ with each of its components equal to 0.0.  It is the
--  identity element under addition.
zero :: Vector
zero = Vector 0.0 0.0 0.0

-- |Add a @Vector@ to a @Vector@ by adding the components pairwise
addVector :: Vector -> Vector -> Vector
addVector (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1+x2) (y1+y2) (z1+z2)

-- |Substract a @Vector@ to a @Vector@ by substracting the components pairwise
subVector :: Vector -> Vector -> Vector
subVector (Vector x1 y1 z1) (Vector x2 y2 z2) = Vector (x1-x2) (y1-y2) (z1-z2)

-- |Negate a @Vector@ by negating each of its components
negVector :: Vector -> Vector
negVector Vector{..} = Vector (-vx) (-vy) (-vz)

-- |Scalar multiplication of a @Vector@ by multiplying each of its components by
--  the scalar
scalarMul :: Vector -> Double -> Vector
scalarMul Vector{..} s = Vector (s*vx) (s*vy) (s*vz)

-- |Scalar division of a @Vector@ by dividing each of its components by
--  the scalar
scalarDiv :: Vector -> Double -> Vector
scalarDiv Vector{..} s = Vector (vx/s) (vy/s) (vz/s)

-- |Compute the magnitude of a @Vector@
magnitude :: Vector -> Double
magnitude Vector{..} = sqrt $ vx**2 + vy**2 + vz**2

-- |Normalize a @Vector@
normalize :: Vector -> Vector
normalize Vector{..} = Vector (vx/m) (vy/m) (vz/m)
    where m = magnitude $ Vector vx vy vz

-- |Compute the dot-product of two @Vector@s
dotProduct :: Vector -> Vector -> Double
dotProduct (Vector x1 y1 z1) (Vector x2 y2 z2) = x1*x2 + y1*y2 + z1*z2

-- |Compute the cross-product of tow @Vector@s
crossProduct :: Vector -> Vector -> Vector
crossProduct (Vector x1 y1 z1) (Vector x2 y2 z2) =
    Vector (y1*z2-y2*z1) (x2*z1-x1*z2) (x1*y2-x2*y1)