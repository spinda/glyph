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

renderGlyph :: Renderable (Path V2 Double) b
            => Glyph -> QDiagram b V2 Double Any
renderGlyph = renderStrokes . glyphStrokes

renderStrokes :: Renderable (Path V2 Double) b
              => [GlyphStroke] -> QDiagram b V2 Double Any
renderStrokes = mconcat . map renderStroke


-- FIXME: Odd notches at ends of some letters ('m', 'n')
-- TODO: Implement connection stroke interpolation
renderStroke :: Renderable (Path V2 Double) b
             => GlyphStroke -> QDiagram b V2 Double Any
renderStroke stroke = (lc black) # (# fc black) $ strokeLocLoop $ (`at` start) $
  loopFromSegments [startCap, rightBezier, endCap, leftBezier] openLinear
  where
    startCap = straight startDelta
    endCap = straight $ negate endDelta ^* 2

    leftBezier = reverseSegment $ unLoc $ renderCurve leftCurve
    rightBezier = unLoc $ renderCurve rightCurve

    leftCurve = curveDragEndpoints curve
      (translate (negate startDelta) (glyphCurveStartPoint curve))
      (translate (negate endDelta) (glyphCurveEndPoint curve))
    rightCurve = curveDragEndpoints curve
      (translate startDelta $ glyphCurveStartPoint curve)
      (translate endDelta $ glyphCurveEndPoint curve)

    startDelta =
      normalize (normalAtStart bezier) ^* (glyphStrokeStartWidth stroke / 2)
    endDelta =
      normalize (normalAtEnd bezier) ^* (glyphStrokeEndWidth stroke / 2)

    Loc start bezier = curveBezier curve
    curve = glyphStrokeCurve stroke

--------------------------------------------------------------------------------
-- Rendering Glyph Collections -------------------------------------------------
--------------------------------------------------------------------------------

renderWord :: Renderable (Path V2 Double) b
           => GlyphWord -> QDiagram b V2 Double Any
renderWord = mconcat . map (flattenLocated . mapLoc renderGlyph)

renderLine :: Renderable (Path V2 Double) b
           => GlyphLine -> QDiagram b V2 Double Any
renderLine = mconcat . map (flattenLocated . mapLoc renderWord)

renderPara :: Renderable (Path V2 Double) b
           => GlyphPara -> QDiagram b V2 Double Any
renderPara = mconcat . map (flattenLocated . mapLoc renderLine)

renderDoc :: Renderable (Path V2 Double) b
           => GlyphDoc -> QDiagram b V2 Double Any
renderDoc = mconcat . map (flattenLocated . mapLoc renderPara)


flattenLocated :: (Num (N t), Transformable t) => Located t -> t
flattenLocated (Loc (P d) x) = translate d x

--------------------------------------------------------------------------------
-- Rendering Cubic Bézier Curves -----------------------------------------------
--------------------------------------------------------------------------------

renderCurve :: GlyphCurve -> Located (Segment Closed V2 Double)
renderCurve = curveBezier

