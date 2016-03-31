-- | Handwriting synthesis and rendering.

{-# LANGUAGE FlexibleContexts #-}

module Longhand.Render (
    -- * Rendering Glyphs
    renderGlyph
  , renderStrokes
  , renderStroke

    -- * Rendering Glyph Combinations
  , renderWord
  , renderLine
  , renderPara
  , renderDoc

    -- * Rendering Cubic Bézier Curves
  , renderCurve
  ) where

import Data.List

import Diagrams.Prelude

import Longhand.Types

--------------------------------------------------------------------------------
-- Rendering Glyphs ------------------------------------------------------------
--------------------------------------------------------------------------------

renderGlyph :: Glyph -> Path V2 Double
renderGlyph = renderStrokes . glyphStrokes

renderStrokes :: [GlyphStroke] -> Path V2 Double
renderStrokes = toPath . map renderStroke

renderStroke :: GlyphStroke -> Located (Segment Closed V2 Double)
renderStroke = renderCurve . glyphStrokeCurve

--------------------------------------------------------------------------------
-- Rendering Glyph Collections -------------------------------------------------
--------------------------------------------------------------------------------

renderWord :: GlyphWord -> Path V2 Double
renderWord = mconcat . map (flattenLocated . mapLoc renderGlyph)

renderLine :: GlyphLine -> Path V2 Double
renderLine = mconcat . map (flattenLocated . mapLoc renderWord)

renderPara :: GlyphPara -> Path V2 Double
renderPara = mconcat . map (flattenLocated . mapLoc renderLine)

renderDoc :: GlyphDoc -> Path V2 Double
renderDoc = mconcat . map (flattenLocated . mapLoc renderPara)


flattenLocated :: (Num (N t), Transformable t) => Located t -> t
flattenLocated (Loc (P d) x) = translate d x

--------------------------------------------------------------------------------
-- Rendering Cubic Bézier Curves -----------------------------------------------
--------------------------------------------------------------------------------

renderCurve :: GlyphCurve -> Located (Segment Closed V2 Double)
renderCurve = curveBezier

