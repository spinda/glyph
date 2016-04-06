-- Copyright (C) 2016 Michael Smith.

-- Glyph is bi-licensed under the Lesser GNU General Public License Version 3
-- or later as well as the Mozilla Public License Version 2. You can modify or
-- redistribute its sources under the conditions of these licenses.

-- The text of the Lesser GNU General Public License Version 3 can found in the
-- COPYING.LGPL3 file, and can also be obtained from
-- <http://www.gnu.org/licenses/>.

-- The text of the Mozilla Public License Version 2 can be found in the
-- COPYING.MPL2 file, and can also be obtained from <http://mozilla.org/MPL/2.0/>.

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
  , emptyStroke
  , mkStrokeStep
  , strokeSteps
  , strokePoints
  , mapStrokeSteps
  , mapStrokePoints
  , mapStrokeStepPoint

    -- * Collections of Glyphs
  , Aligned
  , GlyphWord
  , GlyphLine
  , GlyphPara
  , GlyphDoc
  ) where

import Glyph.Geometry
import Glyph.Serialize
import Glyph.Types.Internal

