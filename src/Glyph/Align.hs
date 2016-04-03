-- Copyright (C) 2016 Michael Smith.

-- Glyph is bi-licensed under the Lesser GNU General Public License Version 3
-- or later as well as the Mozilla Public License Version 2. You can modify or
-- redistribute its sources under the conditions of these licenses.

-- The text of the Lesser GNU General Public License Version 3 can found in the
-- COPYING.LGPL3 file, and can also be obtained from
-- <http://www.gnu.org/licenses/>.

-- The text of the Mozilla Public License Version 2 can be found in the
-- COPYING.MPL2 file, and can also be obtained from <http://mozilla.org/MPL/2.0/>.

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

