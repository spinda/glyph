-- | Lay out glyphs, words, etc. next to one another.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Longhand.Layout (
    -- * Glyph Collection Layout
    layoutWord

    -- * Pair-wise Alignment Functions
  , alignGlyph
  ) where

import Data.Data
import Data.List
import Data.Typeable

import Diagrams.Prelude

import GHC.Generics

import Longhand.Types

--------------------------------------------------------------------------------
-- Glyph Collection Layout -----------------------------------------------------
--------------------------------------------------------------------------------

layoutWord :: RawWord -> GlyphWord
layoutWord [] = []
layoutWord (x:xs) = scanl' alignGlyph (Loc zero x) xs

--------------------------------------------------------------------------------
-- Pair-wise Alignment Functions -----------------------------------------------
--------------------------------------------------------------------------------

alignGlyph :: Located Glyph -> Glyph -> Located Glyph
alignGlyph g1 g2 = Loc delta g2
  where
    delta = mkP2 (g1_maxX - g2_minX) 0
    g1_maxX = fst $ unp2 $ envelopeP unitX $ getEnvelope g1
    g2_minX = fst $ unp2 $ envelopeP unit_X $ getEnvelope g2

