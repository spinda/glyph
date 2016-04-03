-- Copyright (C) 2016 Michael Smith.

-- Glyph is bi-licensed under the Lesser GNU General Public License Version 3
-- or later as well as the Mozilla Public License Version 2. You can modify or
-- redistribute its sources under the conditions of these licenses.

-- The text of the Lesser GNU General Public License Version 3 can found in the
-- COPYING.LGPL3 file, and can also be obtained from
-- <http://www.gnu.org/licenses/>.

-- The text of the Mozilla Public License Version 2 can be found in the
-- COPYING.MPL2 file, and can also be obtained from <http://mozilla.org/MPL/2.0/>.

-- | Randomly perturb source glyphs to produce unique derivative glyphs, to
-- produce a more natural handwriting feel.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Glyph.Perturb (
    -- * Randomly Perturb Glyphs
    perturbGlyph
  , perturbGlyph'

    -- * Warp Glyphs with Period Wave Functions
  , warpPoint
  , warpStroke
  , warpGlyph

    -- * Periodic Wave Generators
  , defaultWaveGenerator
  ) where

import Data.Bifunctor
import Data.List
import Data.Tuple

import Diagrams.Prelude

import System.Random

import Glyph.Geometry
import Glyph.Types

--------------------------------------------------------------------------------
-- Randomly Perturb Glyphs -----------------------------------------------------
--------------------------------------------------------------------------------

perturbGlyph :: RandomGen r => Double -> r -> Glyph -> (r, Glyph)
perturbGlyph = perturbGlyph' . defaultWaveGenerator

perturbGlyph' :: RandomGen r => (r -> (r, Double -> Double)) -> r
              -> Glyph -> (r, Glyph)
perturbGlyph' waveGenerator rand glyph =
  second (`warpGlyph` glyph) $ waveGenerator rand

--------------------------------------------------------------------------------
-- Warp Glyphs with Periodic Wave Functions ------------------------------------
--------------------------------------------------------------------------------

warpPoint :: (Double -> Double) -> P2 Double -> P2 Double -> P2 Double
warpPoint wave centroid point = relPoint ^* wave angle ^+^ centroid
  where
    angle        = atan2 relX relY
    (relX, relY) = unp2 relPoint
    relPoint     = point ^-^ centroid

warpStroke :: (Double -> Double) -> P2 Double -> Stroke -> Stroke
warpStroke wave centroid = mapStrokePoints (warpPoint wave centroid)

warpGlyph :: (Double -> Double) -> Glyph -> Glyph
warpGlyph wave glyph =
  mapGlyphPoints (warpPoint wave $ glyphCentroid glyph) glyph

--------------------------------------------------------------------------------
-- Periodic Wave Generators ----------------------------------------------------
--------------------------------------------------------------------------------

defaultWaveGenerator :: RandomGen r => Double -> r -> (r, Double -> Double)
defaultWaveGenerator scale rand = (rand', wave)
  where
    wave t = exp $ (scale *) $ sum $ map (order t) coefficients
    order t (i, (a, b)) = a * cos (i * t) + b * sin ((i + 1) * t)

    coefficients = zip [0..] $ zip (take 3 xs) (drop 3 xs)
    (rand', xs) = mapAccumL (\r _ -> swap $ randomR (0, 1) r) rand [1..6]

