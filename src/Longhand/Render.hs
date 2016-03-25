-- | Handwriting synthesis and rendering.

module Longhand.Render (
    -- * Rendering Glyphs
    renderGlyph
  , renderGlyphSegment

    -- * Rendering Cubic Bézier Curves
  , renderCubicCurve
  ) where

import Diagrams.Prelude

import Longhand.Glyphs

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
renderCubicCurve (CubicCurve p@(P st) (P c1) (P c2) (P ed)) =
  Loc p $ bezier3 (c1 ^-^ st) (c2 ^-^ st) (ed ^-^ st)

