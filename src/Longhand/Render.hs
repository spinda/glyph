-- | Handwriting synthesis and rendering.

module Longhand.Render (
    -- * Glyph Rendering
    renderGlyph
  , renderGlyphSegment
  , renderGlyphCurve
  ) where

import Diagrams.Prelude

import Longhand.Glyphs

--------------------------------------------------------------------------------
-- Glyph Rendering -------------------------------------------------------------
--------------------------------------------------------------------------------

renderGlyph :: Glyph -> Path V2 Double
renderGlyph = toPath . map renderGlyphSegment . glyphSegments

renderGlyphSegment :: GlyphSegment -> Located (Segment Closed V2 Double)
renderGlyphSegment = renderGlyphCurve . glyphSegmentCurve

renderGlyphCurve :: GlyphCurve -> Located (Segment Closed V2 Double)
renderGlyphCurve (GlyphCurve p@(P st) (P c1) (P c2) (P ed)) =
  Loc p $ bezier3 (c1 ^-^ st) (c2 ^-^ st) (ed ^-^ st)

