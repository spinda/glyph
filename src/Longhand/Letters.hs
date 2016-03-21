-- | Letter form representation and manipulation.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Longhand.Letters (
    -- * Map from Char to Letter Form Data
    LetterMap

    -- * Letter Form Representation
  , Letter(..)
  , LetterKind(..)
  , LetterSegment(..)
  ) where

import Data.Data
import Data.Typeable

import Data.CharMap.Strict (CharMap)

import GHC.Generics

import Longhand.Geometry

--------------------------------------------------------------------------------
-- Map from Char to Letter Form Data -------------------------------------------
--------------------------------------------------------------------------------

type LetterMap = CharMap Letter

--------------------------------------------------------------------------------
-- Letter Form Representation --------------------------------------------------
--------------------------------------------------------------------------------

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

