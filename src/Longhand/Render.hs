-- | Handwriting synthesis and rendering.

module Longhand.Render (
    -- * Rendering Chunks
    renderChunk

    -- * Rendering Glyphs
  , renderGlyph
  , renderGlyphSegment

    -- * Rendering Cubic Bézier Curves
  , renderCubicCurve
  ) where

import Data.List

import Diagrams.Prelude

import Longhand.Chunks
import Longhand.Glyphs

--------------------------------------------------------------------------------
-- Rendering Chunks ------------------------------------------------------------
--------------------------------------------------------------------------------

renderChunk :: Chunk -> Path V2 Double
renderChunk = mconcat . map renderGlyph . chunkGlyphs

--------------------------------------------------------------------------------
-- Rendering Glyphs ------------------------------------------------------------
--------------------------------------------------------------------------------

renderGlyph :: Glyph -> Path V2 Double
renderGlyph = toPath . map renderGlyphSegment . glyphSegments

renderGlyphSegment :: GlyphSegment -> Located (Segment Closed V2 Double)
renderGlyphSegment = renderCubicCurve . glyphSegmentCurve

--------------------------------------------------------------------------------
-- Rendering Cubic Bézier Curves -----------------------------------------------
--------------------------------------------------------------------------------

renderCubicCurve :: CubicCurve -> Located (Segment Closed V2 Double)
renderCubicCurve = curveBezier

