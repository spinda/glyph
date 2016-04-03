-- | Randomly perturb source glyphs to produce unique derivative glyphs, to
-- produce a more natural handwriting feel.

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Longhand.Perturb (
    -- * Randomly Perturb Glyphs
    perturbGlyph
  , perturbGlyph'
  , perturbGlyphs
  , PerturbGlyphs(..)

    -- * Warp Glyphs with Period Wave Functions
  , warpPoint
  , warpStroke
  , warpGlyph
  , WarpGlyphs(..)

    -- * Periodic Wave Generators
  , defaultWaveGenerator
  ) where

import Data.Bifunctor
import Data.List
import Data.Tuple

import Diagrams.Prelude

import System.Random

import Longhand.Types

--------------------------------------------------------------------------------
-- Randomly Perturb Glyphs -----------------------------------------------------
--------------------------------------------------------------------------------

perturbGlyph :: RandomGen r => Double -> r -> Glyph -> (r, Glyph)
perturbGlyph = perturbGlyph' . defaultWaveGenerator

perturbGlyph' :: RandomGen r => (r -> (r, Double -> Double)) -> r
              -> Glyph -> (r, Glyph)
perturbGlyph' waveGenerator rand glyph =
  second (`warpGlyph` glyph) $ waveGenerator rand


perturbGlyphs :: (PerturbGlyphs a, RandomGen r) => Double -> r -> a -> (r, a)
perturbGlyphs = perturbGlyphs' . defaultWaveGenerator

class PerturbGlyphs a where
  perturbGlyphs' :: RandomGen r => (r -> (r, Double -> Double)) -> r -> a -> (r, a)

instance PerturbGlyphs GlyphWord where
  perturbGlyphs' = mapAccumL . perturbGlyph'

instance PerturbGlyphs a => PerturbGlyphs [a] where
  perturbGlyphs' = mapAccumL . perturbGlyphs'

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


class WarpGlyphs a where
  warpGlyphs :: (Double -> Double) -> a -> a

instance WarpGlyphs GlyphWord where
  warpGlyphs = map . warpGlyph

instance WarpGlyphs a => WarpGlyphs [a] where
  warpGlyphs = map . warpGlyphs

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

