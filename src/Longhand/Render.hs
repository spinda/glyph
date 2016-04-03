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
  ) where

import Data.List

import Diagrams.Prelude

import Longhand.Types

--------------------------------------------------------------------------------
-- Rendering Glyphs ------------------------------------------------------------
--------------------------------------------------------------------------------

renderGlyph :: Renderable (Path V2 Double) b
            => Glyph -> QDiagram b V2 Double Any
renderGlyph = renderStrokes . glyphStrokes

renderStrokes :: Renderable (Path V2 Double) b
              => [Stroke] -> QDiagram b V2 Double Any
renderStrokes = mconcat . map renderStroke

renderStroke :: Renderable (Path V2 Double) b
             => Stroke -> QDiagram b V2 Double Any
renderStroke = strokeP . cubicSpline False . strokePoints

--------------------------------------------------------------------------------
-- Rendering Glyph Collections -------------------------------------------------
--------------------------------------------------------------------------------

renderWord :: Renderable (Path V2 Double) b
           => Arranged GlyphWord -> QDiagram b V2 Double Any
renderWord = mconcat . map (flattenLocated . mapLoc renderGlyph)

renderLine :: Renderable (Path V2 Double) b
           => Arranged GlyphLine -> QDiagram b V2 Double Any
renderLine = mconcat . map (flattenLocated . mapLoc renderWord)

renderPara :: Renderable (Path V2 Double) b
           => Arranged GlyphPara -> QDiagram b V2 Double Any
renderPara = mconcat . map (flattenLocated . mapLoc renderLine)

renderDoc :: Renderable (Path V2 Double) b
           => Arranged GlyphDoc -> QDiagram b V2 Double Any
renderDoc = mconcat . map (flattenLocated . mapLoc renderPara)


flattenLocated :: (Num (N t), Transformable t) => Located t -> t
flattenLocated (Loc (P d) x) = translate d x

