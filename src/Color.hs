{-# LANGUAGE RecordWildCards #-}

module Color
    ( Color(..)
    , addColor
    , subColor
    , scalarMul
    , mulColor
    , hadamar
    ) where

import Internal.Types (Color(..))

-- |Add a @Color@ to a @Color@ by adding the components pairwise
addColor :: Color -> Color -> Color
addColor (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1+r2) (g1+g2) (b1+b2)

-- |Substract a @Color@ to a @Color@ by substracting the components pairwise
subColor :: Color -> Color -> Color
subColor (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1-r2) (g1-g2) (b1-b2)

-- |Scalar multiplication of a @Color@ by multiplying each of its components by
--  the scalar
scalarMul :: Color -> Double -> Color
scalarMul Color{..} s = Color (s*red) (s*green) (s*blue)

-- |Multiply a @Color@ to a @Color@ by multiplying the components pairwise
mulColor :: Color -> Color -> Color
mulColor (Color r1 g1 b1) (Color r2 g2 b2) = Color (r1*r2) (g1*g2) (b1*b2)

-- |Hadamar Product of a @Color@
hadamar :: Color -> Color -> Color
hadamar = mulColor