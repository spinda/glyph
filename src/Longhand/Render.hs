-- | Handwriting synthesis and rendering.

{-# LANGUAGE FlexibleContexts #-}

module Longhand.Render (
    -- * Rendering Glyphs
    renderGlyph
  , renderGlyphSegments
  , renderGlyphSegment

    -- * Rendering Glyph Combinations
  , renderWord
  , renderLine
  , renderPara
  , renderDoc

    -- * Rendering Cubic Bézier Curves
  , renderGlyphCurve
  ) where

import Data.List

import Diagrams.Prelude

import Longhand.Types

--------------------------------------------------------------------------------
-- Rendering Glyphs ------------------------------------------------------------
--------------------------------------------------------------------------------

renderGlyph :: Glyph -> Path V2 Double
renderGlyph = renderGlyphSegments . glyphSegments

renderGlyphSegments :: [GlyphSegment] -> Path V2 Double
renderGlyphSegments = toPath . map renderGlyphSegment

renderGlyphSegment :: GlyphSegment -> Located (Segment Closed V2 Double)
renderGlyphSegment = renderGlyphCurve . glyphSegmentCurve

--------------------------------------------------------------------------------
-- Rendering Glyph Collections -------------------------------------------------
--------------------------------------------------------------------------------

renderWord :: GlyphWord -> Path V2 Double
renderWord = mconcat . map (collapseLocated . mapLoc renderGlyph)

renderLine :: GlyphLine -> Path V2 Double
renderLine = mconcat . map (collapseLocated . mapLoc renderWord)

renderPara :: GlyphPara -> Path V2 Double
renderPara = mconcat . map (collapseLocated . mapLoc renderLine)

renderDoc :: GlyphDoc -> Path V2 Double
renderDoc = mconcat . map (collapseLocated . mapLoc renderPara)


collapseLocated :: (Num (N t), Transformable t) => Located t -> t
collapseLocated (Loc (P d) x) = translate d x

--------------------------------------------------------------------------------
-- Rendering Cubic Bézier Curves -----------------------------------------------
--------------------------------------------------------------------------------

renderGlyphCurve :: GlyphCurve -> Located (Segment Closed V2 Double)
renderGlyphCurve = curveBezier

