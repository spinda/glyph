-- | Shared type definitions.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Longhand.Types (
    -- * Map from Char to Glyph Data
    GlyphMap

    -- * Glyph Representation
  , Glyph(..)
  , GlyphKind(..)
  , GlyphSegment(..)
  , mapGlyphSegments
  , mapGlyphSegmentCurve
  , glyphCentroid

    -- * Cubic Bézier Curves
  , GlyphCurve(..)
  , mapCurvePoints
  , curveBezier
  , curveLinearLength
  , curveCentroid
  , curveDragEndpoints

    -- * Combinations of Glyphs
    -- ** Raw, Unprocessed Glyph Input
  , RawWord
  , RawLine
  , RawPara
  , RawDoc
    -- ** Processed Glyph Output (Post-Layout)
  , GlyphWord
  , GlyphLine
  , GlyphPara
  , GlyphDoc
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


type instance V Glyph = V2
type instance V GlyphSegment = V2

type instance N Glyph = Double
type instance N GlyphSegment = Double

instance Transformable Glyph where
  transform = mapGlyphSegments . transform

instance Transformable GlyphSegment where
  transform = mapGlyphSegmentCurve . transform

instance Enveloped Glyph where
  getEnvelope = mconcat . map getEnvelope . glyphSegments

instance Enveloped GlyphSegment where
  getEnvelope = getEnvelope . glyphSegmentCurve


mapGlyphSegments :: (GlyphSegment -> GlyphSegment) -> Glyph -> Glyph
mapGlyphSegments f g = g { glyphSegments = f <$> glyphSegments g }

mapGlyphSegmentCurve :: (GlyphCurve -> GlyphCurve)
                     -> GlyphSegment -> GlyphSegment
mapGlyphSegmentCurve f g = g { glyphSegmentCurve = f $ glyphSegmentCurve g }


glyphCentroid :: Glyph -> P2 Double
glyphCentroid g = sumV (curveCentroid <$> curves)
  where
    curves = glyphSegmentCurve <$> glyphSegments g

--------------------------------------------------------------------------------
-- Cubic Bézier Curves ---------------------------------------------------------
--------------------------------------------------------------------------------

data GlyphCurve = GlyphCurve
  { glyphCurveStartPoint    :: !(P2 Double)
  , glyphCurveControlPoint1 :: !(P2 Double)
  , glyphCurveControlPoint2 :: !(P2 Double)
  , glyphCurveEndPoint      :: !(P2 Double)
  } deriving (Eq, Show, Data, Typeable, Generic)


type instance V GlyphCurve = V2
type instance N GlyphCurve = Double

instance Transformable GlyphCurve where
  transform = mapCurvePoints . papply

instance Enveloped GlyphCurve where
  getEnvelope = getEnvelope . mapLoc getEnvelope . curveBezier


mapCurvePoints :: (P2 Double -> P2 Double) -> GlyphCurve -> GlyphCurve
mapCurvePoints f (GlyphCurve st c1 c2 ed) =
  GlyphCurve (f st) (f c1) (f c2) (f ed)


curveBezier :: GlyphCurve -> Located (Segment Closed V2 Double)
curveBezier (GlyphCurve p@(P st) (P c1) (P c2) (P ed)) =
  Loc p $ bezier3 (c1 ^-^ st) (c2 ^-^ st) (ed ^-^ st)

curveLinearLength :: GlyphCurve -> Double
curveLinearLength (GlyphCurve st _ _ ed) = distance st ed

curveCentroid :: GlyphCurve -> P2 Double
curveCentroid (GlyphCurve st c1 c2 ed) = centroid [st, c1, c2, ed]

curveDragEndpoints :: GlyphCurve -> P2 Double -> P2 Double -> GlyphCurve
curveDragEndpoints gc@(GlyphCurve st c1 c2 ed) st' ed' =
  GlyphCurve st' c1' c2' ed'
  where
    scale = distance st' ed' / curveLinearLength gc
    c1' = (c1 ^-^ st) ^* scale ^+^ st'
    c2' = (c2 ^-^ ed) ^* scale ^+^ ed'

--------------------------------------------------------------------------------
-- Combinations of Glyphs ------------------------------------------------------
--------------------------------------------------------------------------------

-- Raw, Unprocessed Glyph Input ------------------------------------------------

type RawWord = [Glyph]
type RawLine = [RawWord]
type RawPara = [RawLine]
type RawDoc  = [RawPara]

-- Processed Glyph Output (Post-Layout) ----------------------------------------

type GlyphWord = [Located Glyph]
type GlyphLine = [Located GlyphWord]
type GlyphPara = [Located GlyphLine]
type GlyphDoc  = [Located GlyphPara]

