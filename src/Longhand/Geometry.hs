-- | Geometry routines and related types.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Longhand.Geometry (
    -- * Cubic Curves
    -- ** Data Type
    CubicCurve(..)
    -- ** Evaluation
  , evalCubicCurve
  , evalDeriv1CubicCurve
  , evalDeriv2CubicCurve
  , evalTangentCubicCurve
  , evalCurvatureCubicCurve
    -- ** Higher-Level Cubic Curve Operations
  , mapCubicCurve
  , translateCubicCurve
  , linearLengthCubicCurve

    -- * 2D Geometric Vector
    -- ** Data Type
  , Vec2(..)
  , zeroVec2
  , upVec2
  , downVec2
  , leftVec2
  , rightVec2
    -- ** Basic Arithmetic Operations
  , (^+)
  , (^-)
  , (^*)
  , (^/)
  , (.*)
    -- ** Higher-Level Vector Operations
  , rotateVec2
  , lengthSqVec2
  , lengthVec2
  , normalizeVec2
  ) where

import Data.Data
import Data.Typeable

import GHC.Generics

--------------------------------------------------------------------------------
-- Cubic Curves ----------------------------------------------------------------
--------------------------------------------------------------------------------

-- Data Type -------------------------------------------------------------------

data CubicCurve = CubicCurve
  { cubicCurveParam1 :: !Vec2
  , cubicCurveParam2 :: !Vec2
  , cubicCurveParam3 :: !Vec2
  , cubicCurveParam4 :: !Vec2
  } deriving (Eq, Show, Data, Typeable, Generic)

-- Evaluation ------------------------------------------------------------------

evalCubicCurve :: CubicCurve -> Double -> Vec2
evalCubicCurve (CubicCurve a b c d) t =
  a ^* u3 ^+ b ^* (3 * u2 * t) ^+ c ^* (3 * u * t2) ^+ d ^* t3
  where
    u = 1 - t
    u2 = u * u
    u3 = u2 * u
    t2 = t * t
    t3 = t2 * t

evalDeriv1CubicCurve :: CubicCurve -> Double -> Vec2
evalDeriv1CubicCurve (CubicCurve a b c d) t =
  (b ^- a) ^* (3 * u2) ^+ (c ^- b) ^* (6 * u * t) ^+ (d ^- c) ^* (3 * t2)
  where
    u = 1 - t
    u2 = u * u
    t2 = t * t

evalDeriv2CubicCurve :: CubicCurve -> Double -> Vec2
evalDeriv2CubicCurve (CubicCurve a b c d) t =
  (c ^- b ^* 2 ^+ a) ^* (6 * u) ^+ (d ^- c ^* 2 ^+ b) ^* (6 * t)
  where
    u = 1 - t
    u2 = u * u
    t2 = t * t


evalTangentCubicCurve :: CubicCurve -> Double -> Vec2
evalTangentCubicCurve cc t = normalizeVec2 $ evalDeriv1CubicCurve cc t

evalCurvatureCubicCurve :: CubicCurve -> Double -> Double
evalCurvatureCubicCurve cc t =
  lengthVec2 (evalDeriv2CubicCurve cc t) / lengthVec2 (evalDeriv2CubicCurve cc t)

-- Higher-Level Cubic Curve Operations -----------------------------------------

mapCubicCurve :: (Vec2 -> Vec2) -> CubicCurve -> CubicCurve
mapCubicCurve f (CubicCurve a b c d) = CubicCurve (f a) (f b) (f c) (f d)

translateCubicCurve :: CubicCurve -> Vec2 -> CubicCurve
translateCubicCurve cc v = mapCubicCurve (^+ v) cc

linearLengthCubicCurve :: CubicCurve -> Double
linearLengthCubicCurve (CubicCurve a _ _ d) = lengthVec2 (d ^- a)

--------------------------------------------------------------------------------
-- 2D Vec2s -------------------------------------------------------------------
--------------------------------------------------------------------------------

-- Data Type -------------------------------------------------------------------

data Vec2 = Vec2
  { vec2X :: {-# UNPACK #-} !Double
  , vec2Y :: {-# UNPACK #-} !Double
  } deriving (Eq, Show, Data, Typeable, Generic)

instance Monoid Vec2 where
  mempty = zeroVec2
  mappend = (^+)

zeroVec2 :: Vec2
zeroVec2 = Vec2 0 0

upVec2 :: Vec2
upVec2 = Vec2 0 (-1)

downVec2 :: Vec2
downVec2 = Vec2 0 1

leftVec2 :: Vec2
leftVec2 = Vec2 (-1) 0

rightVec2 :: Vec2
rightVec2 = Vec2 1 0

-- Basic Arithmetic Operations -------------------------------------------------

(^+) :: Vec2 -> Vec2 -> Vec2
(^+) a b = Vec2
  { vec2X = vec2X a + vec2X b
  , vec2Y = vec2Y a + vec2Y b
  }

(^-) :: Vec2 -> Vec2 -> Vec2
(^-) a b = Vec2
  { vec2X = vec2X a - vec2X b
  , vec2Y = vec2Y a - vec2Y b
  }

(^*) :: Vec2 -> Double -> Vec2
(^*) v s = Vec2
  { vec2X = vec2X v * s
  , vec2Y = vec2Y v * s
  }

(^/) :: Vec2 -> Double -> Vec2
(^/) v s = Vec2
  { vec2X = vec2X v / s
  , vec2Y = vec2Y v / s
  }

(.*) :: Vec2 -> Vec2 -> Double
(.*) a b = vec2X a * vec2X a + vec2Y b * vec2Y b

infixl 6 ^+
infixl 6 ^-
infixl 7 ^*
infixl 7 ^/
infixl 7 .*

-- Higher-Level Vector Operations ----------------------------------------------

rotateVec2 :: Double -> Vec2 -> Vec2
rotateVec2 angle (Vec2 x y) = Vec2
  { vec2X = x * x' - y * y'
  , vec2Y = x * y' + y * x'
  }
  where
    x' = cos angle
    y' = sin angle

lengthSqVec2 :: Vec2 -> Double
lengthSqVec2 (Vec2 x y) = x * x + y * y

lengthVec2 :: Vec2 -> Double
lengthVec2 = sqrt . lengthSqVec2

normalizeVec2 :: Vec2 -> Vec2
normalizeVec2 v = v ^/ lengthVec2 v

