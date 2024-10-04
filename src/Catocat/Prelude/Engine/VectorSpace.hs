{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Catocat.Prelude.Engine.VectorSpace where

import Catocat.Prelude.Internal

import Data.VectorSpace (
    VectorSpace (dot, negateVector, zeroVector, (*^), (^+^), (^-^)),
 )
import Linear as L (
    Additive (zero, (^+^), (^-^)),
    Metric (dot),
    V2,
    negated,
    (*^),
 )


-- | This will be used by Linear and Raylib's Vector2 (alias to Linear's V2).
instance (Eq a, Floating a) => VectorSpace (V2 a) a where
    zeroVector = L.zero
    (*^) = (L.*^)
    negateVector = L.negated
    (^+^) = (L.^+^)
    (^-^) = (L.^-^)
    dot = L.dot
