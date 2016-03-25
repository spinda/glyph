-- | Randomly perturb source glyphs to produce unique derivative glyphs, to
-- produce a more natural handwriting feel.

module Longhand.Perturb (
    -- * Randomly Perturb Glyphs
    perturbGlyph
  ) where

import Diagrams.Prelude

import System.Random

import Longhand.Glyphs

--------------------------------------------------------------------------------
-- Randomly Perturb Glyphs -----------------------------------------------------
--------------------------------------------------------------------------------

perturbGlyph :: RandomGen r => r -> Double -> Glyph -> (Glyph, r)
perturbGlyph rand scale glyph = (warpGlyph wave glyph, rand')
  where
    wave = sineWave pairs scale
    (pairs, rand') = randomPairs 3 rand

randomPairs :: RandomGen r => Int -> r -> ([(Double, Double)], r)
randomPairs n r = go n r []
  where
    go 0 r ps = (ps, r)
    go n r ps = let (x, r' ) = randomR (0, 1) r
                    (y, r'') = randomR (0, 1) r'
                in  go (n - 1) r'' ((x, y) : ps)

--------------------------------------------------------------------------------
-- Warp Glyphs & Glyph Components ----------------------------------------------
--------------------------------------------------------------------------------

warpGlyph :: (Double -> Double) -> Glyph -> Glyph
warpGlyph wave glyph = glyph
  { glyphSegments = warpSegment wave centroid <$> glyphSegments glyph }
  where
    centroid = glyphCenterOfMass glyph

warpSegment :: (Double -> Double) -> P2 Double -> GlyphSegment -> GlyphSegment
warpSegment wave centroid segment = segment
  { glyphSegmentCurve = warpCurve wave centroid $ glyphSegmentCurve segment }

warpCurve :: (Double -> Double) -> P2 Double -> CubicCurve -> CubicCurve
warpCurve wave centroid curve = dragEndpoints curve st' ed'
  where
    st' = warpPoint wave centroid $ cubicCurveStartPoint curve
    ed' = warpPoint wave centroid $ cubicCurveEndPoint curve

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

