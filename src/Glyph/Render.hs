-- Copyright (C) 2016 Michael Smith.

-- Glyph is bi-licensed under the Lesser GNU General Public License Version 3
-- or later as well as the Mozilla Public License Version 2. You can modify or
-- redistribute its sources under the conditions of these licenses.

-- The text of the Lesser GNU General Public License Version 3 can found in the
-- COPYING.LGPL3 file, and can also be obtained from
-- <http://www.gnu.org/licenses/>.

-- The text of the Mozilla Public License Version 2 can be found in the
-- COPYING.MPL2 file, and can also be obtained from <http://mozilla.org/MPL/2.0/>.

-- | Handwriting synthesis and rendering.

{-# LANGUAGE FlexibleContexts #-}

module Glyph.Render (
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

import Glyph.Types

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
           => Aligned GlyphWord -> QDiagram b V2 Double Any
renderWord = mconcat . map (flattenLocated . mapLoc renderGlyph)

renderLine :: Renderable (Path V2 Double) b
           => Aligned GlyphLine -> QDiagram b V2 Double Any
renderLine = mconcat . map (flattenLocated . mapLoc renderWord)

renderPara :: Renderable (Path V2 Double) b
           => Aligned GlyphPara -> QDiagram b V2 Double Any
renderPara = mconcat . map (flattenLocated . mapLoc renderLine)

renderDoc :: Renderable (Path V2 Double) b
           => Aligned GlyphDoc -> QDiagram b V2 Double Any
renderDoc = mconcat . map (flattenLocated . mapLoc renderPara)


flattenLocated :: (Num (N t), Transformable t) => Located t -> t
flattenLocated (Loc (P d) x) = translate d x

