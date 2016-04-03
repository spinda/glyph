-- | Make connections between glyphs in words.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Longhand.Connect (
    -- * Merge Strokes
    mergeStrokes

    -- * Connect Glyphs and Words
  , connectGlyphs
  , connectWord
  , ConnectWords(..)
  ) where

import Diagrams.Prelude

import Longhand.Types

--------------------------------------------------------------------------------
-- Merge Strokes ---------------------------------------------------------------
--------------------------------------------------------------------------------

mergeStrokes :: Stroke -> Stroke -> Stroke
mergeStrokes = undefined

--------------------------------------------------------------------------------
-- Connect Glyphs and Words ----------------------------------------------------
--------------------------------------------------------------------------------

connectGlyphs :: Arranged Glyph -> Arranged Glyph
              -> (Arranged Glyph, Arranged Glyph)
connectGlyphs = undefined

connectWord :: Arranged GlyphWord -> Arranged GlyphWord
connectWord [] = []
connectWord (g:gs) = go g gs
  where
    go g1 [] = [g1]
    go g1 (g2:gs) =
      let (g1', g2') = connectGlyphs g1 g2
      in  g1' : go g2' gs


class ConnectWords a where
  connectWords :: a -> a

instance ConnectWords [[Located Glyph]] where
  connectWords = map connectWord

instance ConnectWords a => ConnectWords [a] where
  connectWords = map connectWords

instance ConnectWords a => ConnectWords (Located a) where
  connectWords = mapLoc connectWords

{-
--------------------------------------------------------------------------------
-- Merge Glyphs and Strokes ----------------------------------------------------
--------------------------------------------------------------------------------

connect :: Aligned Glyph -> Aligned Glyph
              -> (Aligned Glyph, Aligned Glyph)
connect l1@(Loc p1@(P v1) g1) l2@(Loc p2@(P v2) g2) =
  case (glyphTail g1, glyphHead g2) of
    (Just s1, Just s2) ->
      ( Loc p1 $ g1
          { glyphTail = Just $ mergeStrokes s1 $ translate (v2 ^-^ v1) s2 }
      , Loc p2 $ g2
          { glyphHead = Nothing }
      )
    _ -> (l1, l2)

mergeStrokes :: GlyphStroke -> GlyphStroke -> GlyphStroke
mergeStrokes g1 g2 = GlyphStroke
  { glyphStrokeCurve      = mergeCurves (glyphStrokeCurve g1) (glyphStrokeCurve g2)
  , glyphStrokeType       = ConnectStroke
  , glyphStrokeStartWidth = glyphStrokeStartWidth g1
  , glyphStrokeEndWidth   = glyphStrokeEndWidth g2
  }

mergeCurves :: GlyphCurve -> GlyphCurve -> GlyphCurve
mergeCurves g1 g2 = GlyphCurve
  { glyphCurveStartPoint    = glyphCurveStartPoint g1
  , glyphCurveControlPoint1 = c1'
  , glyphCurveControlPoint2 = c2'
  , glyphCurveEndPoint      = glyphCurveEndPoint g2
  }
  where
    startTangent = glyphCurveControlPoint1 g1 ^-^ glyphCurveStartPoint g1
    endTangent   = glyphCurveControlPoint2 g2 ^-^ glyphCurveEndPoint   g2
    scale        = distance (glyphCurveStartPoint g1) (glyphCurveEndPoint g2)
    c1'          = glyphCurveStartPoint g1 ^+^ startTangent ^* scale
    c2'          = glyphCurveEndPoint   g2 ^+^ endTangent   ^* scale

-}
