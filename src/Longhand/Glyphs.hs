-- | Glyph representation and manipulation.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Longhand.Glyphs (
    -- * Map from Char to Glyph Data
    GlyphMap

    -- * Glyph Representation
  , Glyph(..)
  , GlyphKind(..)
  , GlyphSegment(..)
  , glyphCentroid
  , mapGlyphSegments
  , mapGlyphCurves
  , mapGlyphSegmentCurve

    -- * Cubic Bézier Curves
  , CubicCurve(..)
  , curveDragEndpoints
  , curveLinearLength
  , curveCentroid
  , curveBezier
  , mapCurvePoints
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
  { glyphSegmentCurve         :: !CubicCurve
  , glyphSegmentStartWidth    :: !Double
  , glyphSegmentEndWidth      :: !Double
  , glyphSegmentAlignTangent  :: !Bool
  , glyphSegmentIsStrokeBreak :: !Bool
  } deriving (Eq, Show, Data, Typeable, Generic)


glyphCentroid :: Glyph -> P2 Double
glyphCentroid g = sumV (curveCentroid <$> curves)
  where
    curves = glyphSegmentCurve <$> glyphSegments g


mapGlyphSegments :: (GlyphSegment -> GlyphSegment) -> Glyph -> Glyph
mapGlyphSegments f g = g
  { glyphSegments = f <$> glyphSegments g }

mapGlyphCurves :: (CubicCurve -> CubicCurve) -> Glyph -> Glyph
mapGlyphCurves f g = g
  { glyphSegments = mapGlyphSegmentCurve f <$> glyphSegments g }

mapGlyphSegmentCurve :: (CubicCurve -> CubicCurve)
                     -> GlyphSegment -> GlyphSegment
mapGlyphSegmentCurve f g = g
  { glyphSegmentCurve = f $ glyphSegmentCurve g }


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

--------------------------------------------------------------------------------
-- Cubic Bézier Curves ---------------------------------------------------------
--------------------------------------------------------------------------------

data CubicCurve = CubicCurve
  { cubicCurveStartPoint    :: !(P2 Double)
  , cubicCurveControlPoint1 :: !(P2 Double)
  , cubicCurveControlPoint2 :: !(P2 Double)
  , cubicCurveEndPoint      :: !(P2 Double)
  } deriving (Eq, Show, Data, Typeable, Generic)


curveDragEndpoints :: CubicCurve -> P2 Double -> P2 Double -> CubicCurve
curveDragEndpoints gc@(CubicCurve st c1 c2 ed) st' ed' =
  CubicCurve st' c1' c2' ed'
  where
    scale = distance st' ed' / curveLinearLength gc
    c1' = (c1 ^-^ st) ^* scale ^+^ st'
    c2' = (c2 ^-^ ed) ^* scale ^+^ ed'

curveLinearLength :: CubicCurve -> Double
curveLinearLength (CubicCurve st _ _ ed) = distance st ed

curveCentroid :: CubicCurve -> P2 Double
curveCentroid (CubicCurve st c1 c2 ed) = centroid [st, c1, c2, ed]

curveBezier :: CubicCurve -> Located (Segment Closed V2 Double)
curveBezier (CubicCurve p@(P st) (P c1) (P c2) (P ed)) =
  Loc p $ bezier3 (c1 ^-^ st) (c2 ^-^ st) (ed ^-^ st)


mapCurvePoints :: (P2 Double -> P2 Double) -> CubicCurve -> CubicCurve
mapCurvePoints f (CubicCurve st c1 c2 ed) =
  CubicCurve (f st) (f c1) (f c2) (f ed)


type instance V CubicCurve = V2
type instance N CubicCurve = Double

instance Transformable CubicCurve where
  transform = mapCurvePoints . papply

instance Enveloped CubicCurve where
  getEnvelope cc = translate offset $ getEnvelope bezier
    where
      (Loc (P offset) bezier) = curveBezier cc

