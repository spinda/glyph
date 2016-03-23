-- | Glyph representation and manipulation.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}

module Longhand.Glyphs (
    -- * Map from Char to Glyph Data
    GlyphMap

    -- * Glyph Representation
  , Glyph(..)
  , GlyphKind(..)
  , GlyphSegment(..)
  , GlyphCurve(..)
  ) where

import Data.Data
import Data.Typeable

import Data.CharMap.Strict (CharMap)

import Diagrams.Prelude

import GHC.Generics

--------------------------------------------------------------------------------
-- Map from Char to Glyph Data -------------------------------------------------
--------------------------------------------------------------------------------

type GlyphMap = CharMap Glyph

--------------------------------------------------------------------------------
-- Glyph Representation --------------------------------------------------------
--------------------------------------------------------------------------------

data Glyph = Glyph
  { glyphKind     :: !GlyphKind
  , glyphSegments :: ![GlyphSegment]
  } deriving (Eq, Show, Data, Typeable, Generic)

data GlyphKind = UpperCaseLetter
               | LowerCaseLetter
               | PunctuationMark
                 deriving (Eq, Show, Enum, Data, Typeable, Generic)

data GlyphSegment = GlyphSegment
  { glyphSegmentCurve         :: !GlyphCurve
  , glyphSegmentStartWidth    :: !Double
  , glyphSegmentEndWidth      :: !Double
  , glyphSegmentAlignTangent  :: !Bool
  , glyphSegmentIsStrokeBreak :: !Bool
  } deriving (Eq, Show, Data, Typeable, Generic)

data GlyphCurve = GlyphCurve
  { glyphCurveStartPoint    :: !(Point V2 Double)
  , glyphCurveControlPoint1 :: !(Point V2 Double)
  , glyphCurveControlPoint2 :: !(Point V2 Double)
  , glyphCurveEndPoint      :: !(Point V2 Double)
  } deriving (Eq, Show, Data, Typeable, Generic)

