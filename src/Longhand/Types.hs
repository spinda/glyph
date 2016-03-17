-- | Core data types.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Longhand.Types (
    Letter(..)
  , LetterKind(..)
  , LetterSegment(..)
  , CubicCurve(..)
  ) where

import Data.Data
import Data.Typeable

import GHC.Generics

data Letter = Letter
  { letterKind     :: !LetterKind
  , letterSegments :: ![LetterSegment]
  } deriving (Show, Data, Typeable, Generic)

data LetterKind = UpperCaseLetter
                | LowerCaseLetter
                | PunctuationMark
                  deriving (Eq, Show, Enum, Data, Typeable, Generic)

data LetterSegment = LetterSegment
  { letterSegmentCurve         :: !CubicCurve
  , letterSegmentStartWidth    :: !Double
  , letterSegmentEndWidth      :: !Double
  , letterSegmentAlignTangent  :: !Bool
  , letterSegmentIsStrokeBreak :: !Bool
  } deriving (Show, Data, Typeable, Generic)

data CubicCurve = CubicCurve
  { cubicCurveParam1 :: !(Double, Double)
  , cubicCurveParam2 :: !(Double, Double)
  , cubicCurveParam3 :: !(Double, Double)
  , cubicCurveParam4 :: !(Double, Double)
  } deriving (Show, Data, Typeable, Generic)

