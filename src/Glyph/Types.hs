-- | Shared type definitions.

module Glyph.Types (
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

import Glyph.Geometry
import Glyph.Serialize
import Glyph.Types.Internal

