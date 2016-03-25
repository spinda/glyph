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
  , glyphCenterOfMass

    -- * Cubic Bézier Curves
  , CubicCurve(..)
  , dragEndpoints
  , straightLineLength
  , curveCenterOfMass
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

glyphCenterOfMass :: Glyph -> P2 Double
glyphCenterOfMass g = sumV (curveCenterOfMass <$> curves)
  where
    curves = glyphSegmentCurve <$> glyphSegments g

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

curveCenterOfMass :: CubicCurve -> P2 Double
curveCenterOfMass (CubicCurve a' b' c' d') =
  mkP2 (avg a1 b1 c1 d1) (avg a2 b2 c2 d2)
  where
    avg a b c d = (1/12)*(3*a + 3*b + c + 3*d)
    (a1, a2) = unp2 a'
    (b1, b2) = unp2 b'
    (c1, c2) = unp2 c'
    (d1, d2) = unp2 d'

