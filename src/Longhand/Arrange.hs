-- | Arrange glyphs and groups of glyphs one after the other.

{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Longhand.Arrange (
    -- * Recursively Arrange Glyph Groups
    Arrange(..)

    -- * Sequentially Arrange Pairs of Glyphs and Glyph Groups
  , ArrangePair(..)
  ) where

import Data.List

import Diagrams.Prelude

import Longhand.Types

--------------------------------------------------------------------------------
-- Recursively Arrange Glyph Groups --------------------------------------------
--------------------------------------------------------------------------------

class Arrange a where
  arrange :: a -> Arranged a

instance Arrange GlyphWord where
  arrange [] = []
  arrange (x:xs) = scanl' arrangePair (Loc zero x) xs

-- XXX: Any way to make this less horrible? Injective type families might help
-- here...

instance ( Arrange a
         , ArrangePair (Arranged a)
         , Arranged [a] ~ [Located (Arranged a)]
         , Num (N (Arranged a))
         , Additive (V (Arranged a))
         ) => Arrange [a] where
  arrange [] = []
  arrange (x:xs) = scanl' arrangePair (Loc zero $ arrange x) (arrange <$> xs)

--------------------------------------------------------------------------------
-- Sequentially Arrange Pairs of Glyphs and Glyph Groups -----------------------
--------------------------------------------------------------------------------

class ArrangePair a where
  arrangePair :: Located a -> a -> Located a

instance ArrangePair Glyph where
  arrangePair g1 g2 = Loc delta g2
    where
      delta = mkP2 (g1_maxX - g2_minX) 0
      g1_maxX = fst $ unp2 $ envelopeP unitX $ getEnvelope g1
      g2_minX = fst $ unp2 $ envelopeP unit_X $ getEnvelope g2

