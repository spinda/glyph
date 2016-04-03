-- Copyright (C) 2016 Michael Smith.

-- Glyph is bi-licensed under the Lesser GNU General Public License Version 3
-- or later as well as the Mozilla Public License Version 2. You can modify or
-- redistribute its sources under the conditions of these licenses.

-- The text of the Lesser GNU General Public License Version 3 can found in the
-- COPYING.LGPL3 file, and can also be obtained from
-- <http://www.gnu.org/licenses/>.

-- The text of the Mozilla Public License Version 2 can be found in the
-- COPYING.MPL2 file, and can also be obtained from <http://mozilla.org/MPL/2.0/>.

-- | Make connections between glyphs in words.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Glyph.Connect (
    -- * Merge Strokes
    mergeStrokes

    -- * Connect Glyphs and Words
  , connectGlyphs
  , connectGlyphWord
  ) where

import Diagrams.Prelude

import Glyph.Types

--------------------------------------------------------------------------------
-- Merge Strokes ---------------------------------------------------------------
--------------------------------------------------------------------------------

mergeStrokes :: Stroke -> Stroke -> Stroke
mergeStrokes s1 s2 = Stroke
  { strokeHead     = strokeHead $ s1
  , strokeBody     = newBody
  , strokeTail     = newTail
  , strokeStartCap = strokeStartCap s1
  , strokeEndCap   = strokeEndCap s2
  }
  where
    (newBody, newTail) = case (strokeBody s2, strokeTail s2) of
      (x : xs, ys) ->
        ( strokeBody s1 ++ (x { strokeStepKind = ConnectionStep }) : xs
        , ys
        )
      ([], y : ys) ->
        ( strokeBody s1
        , (y { strokeStepKind = ConnectionStep}) : ys
        )
      ([], []) ->
        ( strokeBody s1
        , []
        )

--------------------------------------------------------------------------------
-- Connect Glyphs and Words ----------------------------------------------------
--------------------------------------------------------------------------------

connectGlyphs :: Aligned Glyph -> Aligned Glyph
              -> (Aligned Glyph, Aligned Glyph)
connectGlyphs l1@(Loc p1@(P v1) g1) l2@(Loc p2@(P v2) g2) =
  case (glyphTail g1, glyphHead g2) of
    (Just s1, Just s2) ->
      ( Loc p1 $ g1
          { glyphTail = Just $ mergeStrokes s1 $ translate (v2 ^-^ v1) s2 }
      , Loc p2 $ g2
          { glyphHead = Nothing }
      )
    _ -> (l1, l2)

connectGlyphWord :: Aligned GlyphWord -> Aligned GlyphWord
connectGlyphWord [] = []
connectGlyphWord (g:gs) = go g gs
  where
    go g1 [] = [g1]
    go g1 (g2:gs) =
      let (g1', g2') = connectGlyphs g1 g2
      in  g1' : go g2' gs

