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
  , GlyphStroke(..)
  , GlyphStrokeType(..)
  , glyphStrokes
  , mapGlyphStrokes
  , mapStrokeCurve
  , glyphCentroid

    -- * Cubic Bézier Curves
  , GlyphCurve(..)
  , mapCurvePoints
  , curveBezier
  , curveLinearLength
  , curveCentroid
  , curveDragEndpoints

    -- * Collections of Glyphs
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
import Data.Maybe
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
  { glyphKind :: !GlyphKind
  , glyphHead :: !(Maybe GlyphStroke)
  , glyphBody :: ![GlyphStroke]
  , glyphTail :: !(Maybe GlyphStroke)
  , glyphHats :: ![GlyphStroke]
  } deriving (Eq, Show, Data, Typeable, Generic)

data GlyphKind = UpperCaseLetter
               | LowerCaseLetter
               | PunctuationMark
                 deriving (Eq, Show, Enum, Data, Typeable, Generic)

data GlyphStroke = GlyphStroke
  { glyphStrokeCurve         :: !GlyphCurve
  , glyphStrokeType          :: !GlyphStrokeType
  , glyphStrokeStartWidth    :: !Double
  , glyphStrokeEndWidth      :: !Double
  } deriving (Eq, Show, Data, Typeable, Generic)

data GlyphStrokeType = LerpStroke
                     | ConnectStroke
                       deriving (Eq, Show, Enum, Data, Typeable, Generic)


type instance V Glyph = V2
type instance V GlyphStroke = V2

type instance N Glyph = Double
type instance N GlyphStroke = Double

instance Transformable Glyph where
  transform = mapGlyphStrokes . transform

instance Transformable GlyphStroke where
  transform = mapStrokeCurve . transform

instance Enveloped Glyph where
  getEnvelope = getEnvelope . glyphStrokes

instance Enveloped GlyphStroke where
  getEnvelope = getEnvelope . glyphStrokeCurve


glyphStrokes :: Glyph -> [GlyphStroke]
glyphStrokes g = concat
  [ maybeToList $ glyphHead g
  , glyphBody g
  , maybeToList $ glyphTail g
  , glyphHats g
  ]

mapGlyphStrokes :: (GlyphStroke -> GlyphStroke) -> Glyph -> Glyph
mapGlyphStrokes f g = g
  { glyphHead = f <$> glyphHead g
  , glyphBody = f <$> glyphBody g
  , glyphTail = f <$> glyphTail g
  , glyphHats = f <$> glyphHats g
  }

mapStrokeCurve :: (GlyphCurve -> GlyphCurve) -> GlyphStroke -> GlyphStroke
mapStrokeCurve f g = g { glyphStrokeCurve = f $ glyphStrokeCurve g }


glyphCentroid :: Glyph -> P2 Double
glyphCentroid g =
  sumV (curveCentroid <$> curves) ^/ fromIntegral (length curves)
  where
    curves = glyphStrokeCurve <$> glyphStrokes g

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
-- Collections of Glyphs -------------------------------------------------------
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

