-- | Geometry routines and related types.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Longhand.Geometry (
    -- * Cubic Curves
    CubicCurve(..)
  ) where

import Data.Data
import Data.Typeable

import GHC.Generics

--------------------------------------------------------------------------------
-- Cubic Curves ----------------------------------------------------------------
--------------------------------------------------------------------------------

data CubicCurve = CubicCurve
  { cubicCurveParam1 :: !(Double, Double)
  , cubicCurveParam2 :: !(Double, Double)
  , cubicCurveParam3 :: !(Double, Double)
  , cubicCurveParam4 :: !(Double, Double)
  } deriving (Show, Data, Typeable, Generic)

