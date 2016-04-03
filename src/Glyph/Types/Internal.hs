-- | Shared type definitions.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Glyph.Types.Internal (
    -- * Map from Char to Glyph Data
    GlyphMap

    -- * Glyph Representation
    -- ** Glyphs
  , Glyph(..)
  , glyphStrokes
  , glyphStrokeSteps
  , glyphPoints
  , mapGlyphStrokes
  , mapGlyphStrokeSteps
  , mapGlyphPoints
    -- ** Strokes
  , Stroke(..)
  , StrokeStep(..)
  , StrokeStepKind(..)
  , StrokeCap(..)
  , strokeSteps
  , strokePoints
  , mapStrokeSteps
  , mapStrokePoints
  , mapStrokeStepPoint

    -- * Collections of Glyphs
  , Arranged
  , GlyphWord
  , GlyphLine
  , GlyphPara
  , GlyphDoc
  ) where

import Data.Data
import Data.Maybe
import Data.List
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

-- Glyphs ----------------------------------------------------------------------

data Glyph = Glyph
  { glyphHead :: !(Maybe Stroke)
  , glyphBody :: ![Stroke]
  , glyphTail :: !(Maybe Stroke)
  , glyphHats :: ![Stroke]
  } deriving (Eq, Show, Data, Typeable, Generic)


glyphStrokes :: Glyph -> [Stroke]
glyphStrokes g = concat
  [ maybeToList $ glyphHead g
  , glyphBody g
  , maybeToList $ glyphTail g
  , glyphHats g
  ]

glyphStrokeSteps :: Glyph -> [StrokeStep]
glyphStrokeSteps = concatMap strokeSteps . glyphStrokes

glyphPoints :: Glyph -> [P2 Double]
glyphPoints = concatMap strokePoints . glyphStrokes

mapGlyphStrokes :: (Stroke -> Stroke) -> Glyph -> Glyph
mapGlyphStrokes f g = g
  { glyphHead = f <$> glyphHead g
  , glyphBody = f <$> glyphBody g
  , glyphTail = f <$> glyphTail g
  , glyphHats = f <$> glyphHats g
  }

mapGlyphStrokeSteps :: (StrokeStep -> StrokeStep) -> Glyph -> Glyph
mapGlyphStrokeSteps f = mapGlyphStrokes (mapStrokeSteps f)

mapGlyphPoints :: (P2 Double -> P2 Double) -> Glyph -> Glyph
mapGlyphPoints f = mapGlyphStrokes (mapStrokePoints f)

-- Strokes ---------------------------------------------------------------------

data Stroke = Stroke
  { strokeHead     :: ![StrokeStep]
  , strokeBody     :: ![StrokeStep]
  , strokeTail     :: ![StrokeStep]
  , strokeStartCap :: !StrokeCap
  , strokeEndCap   :: !StrokeCap
  } deriving (Eq, Show, Data, Typeable, Generic)

data StrokeStep = StrokeStep
  { strokeStepPoint :: !(P2 Double)
  , strokeStepWidth :: !Double
  , strokeStepKind  :: !StrokeStepKind
  } deriving (Eq, Show, Data, Typeable, Generic)

data StrokeStepKind = NormalStep | ConnectionStep
                      deriving (Eq, Show, Enum, Data, Typeable, Generic)

data StrokeCap = SharpCap | RoundCap | BevelCap
                 deriving (Eq, Show, Data, Typeable, Generic)

strokeSteps :: Stroke -> [StrokeStep]
strokeSteps stroke = concat
  [ strokeHead stroke
  , strokeBody stroke
  , strokeTail stroke
  ]

strokePoints :: Stroke -> [P2 Double]
strokePoints = (strokeStepPoint <$>) . strokeSteps

mapStrokeSteps :: (StrokeStep -> StrokeStep) -> Stroke -> Stroke
mapStrokeSteps f stroke = stroke
  { strokeHead = f <$> strokeHead stroke
  , strokeBody = f <$> strokeBody stroke
  , strokeTail = f <$> strokeTail stroke
  }

mapStrokePoints :: (P2 Double -> P2 Double) -> Stroke -> Stroke
mapStrokePoints f = mapStrokeSteps (mapStrokeStepPoint f)

mapStrokeStepPoint :: (P2 Double -> P2 Double)
               -> StrokeStep -> StrokeStep
mapStrokeStepPoint f handle = handle
  { strokeStepPoint = f $ strokeStepPoint handle
  }

--------------------------------------------------------------------------------
-- Collections of Glyphs -------------------------------------------------------
--------------------------------------------------------------------------------

type family Arranged a where
  Arranged [[a]] = [Located (Arranged [a])]
  Arranged [a] = [Arranged a]
  Arranged a = Located a

type GlyphWord = [Glyph]
type GlyphLine = [GlyphWord]
type GlyphPara = [GlyphLine]
type GlyphDoc  = [GlyphPara]

