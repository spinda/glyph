-- | Randomly perturb source glyphs to produce unique derivative glyphs, to
-- produce a more natural handwriting feel.

module Longhand.Perturb (
    -- * Randomly Perturb Glyphs
    perturbGlyph
  , perturbWord
  , perturbLine
  , perturbPara
  , perturbDoc
  ) where

import Data.List

import Diagrams.Prelude

import System.Random

import Longhand.Types

--------------------------------------------------------------------------------
-- Randomly Perturb Glyphs -----------------------------------------------------
--------------------------------------------------------------------------------

perturbGlyph :: RandomGen r => Double -> r -> Glyph -> (r, Glyph)
perturbGlyph scale rand glyph = (rand', warpGlyph wave glyph)
  where
    wave = sineWave pairs scale
    (rand', pairs) = randomPairs 3 rand

randomPairs :: RandomGen r => Int -> r -> (r, [(Double, Double)])
randomPairs n r = mapAccumL go r [1..n]
  where
    go r _ = let (x, r' ) = randomR (0, 1) r
                 (y, r'') = randomR (0, 1) r'
             in  (r'', (x, y))


perturbWord :: RandomGen r => Double -> r -> RawWord -> (r, RawWord)
perturbWord scale = mapAccumL (perturbGlyph scale)

perturbLine :: RandomGen r => Double -> r -> RawLine -> (r, RawLine)
perturbLine scale = mapAccumL (perturbWord scale)

perturbPara :: RandomGen r => Double -> r -> RawPara -> (r, RawPara)
perturbPara scale = mapAccumL (perturbLine scale)

perturbDoc :: RandomGen r => Double -> r -> RawDoc -> (r, RawDoc)
perturbDoc scale = mapAccumL (perturbPara scale)

--------------------------------------------------------------------------------
-- Warp Glyphs & Glyph Components ----------------------------------------------
--------------------------------------------------------------------------------

warpGlyph :: (Double -> Double) -> Glyph -> Glyph
warpGlyph wave glyph = mapGlyphStrokes (warpStroke wave centroid) glyph
  where
    centroid = glyphCentroid glyph

warpStroke :: (Double -> Double) -> P2 Double -> GlyphStroke -> GlyphStroke
warpStroke wave centroid = mapStrokeCurve (warpCurve wave centroid)

warpCurve :: (Double -> Double) -> P2 Double -> GlyphCurve -> GlyphCurve
warpCurve wave centroid curve = curveDragEndpoints curve st' ed'
  where
    st' = warpPoint wave centroid $ glyphCurveStartPoint curve
    ed' = warpPoint wave centroid $ glyphCurveEndPoint curve

warpPoint :: (Double -> Double) -> P2 Double -> P2 Double -> P2 Double
warpPoint wave centroid point = relPoint ^* wave angle ^+^ centroid
  where
    angle        = atan2 relX relY
    (relX, relY) = unp2 relPoint
    relPoint     = point ^-^ centroid

--------------------------------------------------------------------------------
-- Periodic Wave Functions -----------------------------------------------------
--------------------------------------------------------------------------------

sineWave :: [(Double, Double)] -> Double -> Double -> Double
sineWave coefficients scale t = exp (scale * value)
  where
    value = sum $ map (uncurry order) $ zip [0..] coefficients
    order i (a, b) = a * cos (i * t) + b * sin ((i + 1) * t)

