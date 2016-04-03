-- | Align glyphs and groups of glyphs one after the other.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Glyph.Align (
    -- * List-wise Alignment Functions
    alignGlyphs

    -- * Pair-wise Alignment Functions
  , alignGlyph
  ) where

import Data.List

import Diagrams.Prelude

import Glyph.Types

--------------------------------------------------------------------------------
-- List-wise Alignment Functions -----------------------------------------------
--------------------------------------------------------------------------------

alignGlyphs :: GlyphWord -> Aligned GlyphWord
alignGlyphs [] = []
alignGlyphs (x:xs) = scanl' alignGlyph (Loc zero x) xs

--------------------------------------------------------------------------------
-- Pair-wise Alignment Functions -----------------------------------------------
--------------------------------------------------------------------------------

alignGlyph :: Aligned Glyph -> Glyph -> Aligned Glyph
alignGlyph g1 g2 = Loc delta g2
  where
    delta = mkP2 (g1_maxX - g2_minX) 0
    g1_maxX = fst $ unp2 $ envelopeP unitX $ getEnvelope g1
    g2_minX = fst $ unp2 $ envelopeP unit_X $ getEnvelope g2

