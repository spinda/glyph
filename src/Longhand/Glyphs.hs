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
  , glyphCentroid
  , glyphEnvelope

    -- * Cubic Bézier Curves
  , CubicCurve(..)
  , dragEndpoints
  , straightLineLength
  , curveCentroid
  , curveEnvelope
  , curveBezier
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

glyphEnvelope :: Glyph -> Envelope V2 Double
glyphEnvelope =
  mconcat . map (curveEnvelope . glyphSegmentCurve) . glyphSegments

--------------------------------------------------------------------------------
-- Cubic Bézier Curves ---------------------------------------------------------
--------------------------------------------------------------------------------

data CubicCurve = CubicCurve
  { cubicCurveStartPoint    :: !(P2 Double)
  , cubicCurveControlPoint1 :: !(P2 Double)
  , cubicCurveControlPoint2 :: !(P2 Double)
  , cubicCurveEndPoint      :: !(P2 Double)
  } deriving (Eq, Show, Data, Typeable, Generic)

dragEndpoints :: CubicCurve -> P2 Double -> P2 Double -> CubicCurve
dragEndpoints gc@(CubicCurve st c1 c2 ed) st' ed' =
  CubicCurve st' c1' c2' ed'
  where
    scale = distance st' ed' / straightLineLength gc
    c1' = (c1 ^-^ st) ^* scale ^+^ st'
    c2' = (c2 ^-^ ed) ^* scale ^+^ ed'

straightLineLength :: CubicCurve -> Double
straightLineLength (CubicCurve st _ _ ed) = distance st ed

curveCentroid :: CubicCurve -> P2 Double
curveCentroid (CubicCurve st c1 c2 ed) = centroid [st, c1, c2, ed]

curveEnvelope :: CubicCurve -> Envelope V2 Double
curveEnvelope cc = moveOriginTo (loc bezier) $ getEnvelope (unLoc bezier)
  where
    bezier = curveBezier cc

curveBezier :: CubicCurve -> Located (Segment Closed V2 Double)
curveBezier (CubicCurve p@(P st) (P c1) (P c2) (P ed)) =
  Loc p $ bezier3 (c1 ^-^ st) (c2 ^-^ st) (ed ^-^ st)

