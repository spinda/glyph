-- | Handwriting synthesis and rendering.

module Longhand.Render (
    -- * Rendering Words
    renderWord

    -- * Rendering Glyphs
  , renderGlyph
  , renderGlyphSegment

    -- * Rendering Cubic Bézier Curves
  , renderCubicCurve
  ) where

import Data.List

import Diagrams.Prelude

import Longhand.Glyphs

--------------------------------------------------------------------------------
-- Rendering Words -------------------------------------------------------------
--------------------------------------------------------------------------------

-- TODO: Track and line up glyph baselines (with jitter in perturbation)
renderWord :: [Glyph] -> Path V2 Double
renderWord = fst . foldl' (uncurry addGlyph) (mempty, 0)

addGlyph :: Path V2 Double -> Double -> Glyph -> (Path V2 Double, Double)
addGlyph path x glyph = (path <> renderGlyph glyph', x + width)
  where
    glyph' = translate delta glyph
    delta = V2 (x - minX) 0
    width = maxX - minX
    minX = fst $ unp2 $ envelopeP unit_X envelope
    maxX = fst $ unp2 $ envelopeP unitX envelope
    envelope = getEnvelope glyph

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

