-- | Geometry routines.

{-# LANGUAGE TypeFamilies #-}

module Glyph.Geometry (
    -- * Find Centroids
    glyphCentroid
  , strokeCentroid
  ) where

import Diagrams.Prelude

import Glyph.Types.Internal

--------------------------------------------------------------------------------
-- Find Centroids --------------------------------------------------------------
--------------------------------------------------------------------------------

glyphCentroid :: Glyph -> P2 Double
glyphCentroid g = sumV centroids ^/ fromIntegral (length centroids)
  where
    centroids = strokeCentroid <$> glyphStrokes g

strokeCentroid :: Stroke -> P2 Double
strokeCentroid = centroid . strokePoints

--------------------------------------------------------------------------------
-- Diagrams Orphan Instances ---------------------------------------------------
--------------------------------------------------------------------------------

-- V and N ---------------------------------------------------------------------

type instance V Glyph = V2
type instance V Stroke = V2
type instance V StrokeStep = V2

type instance N Glyph = Double
type instance N Stroke = Double
type instance N StrokeStep = Double

-- Enveloped -------------------------------------------------------------------

instance Enveloped Glyph where
  getEnvelope = getEnvelope . glyphStrokes

instance Enveloped Stroke where
  getEnvelope = getEnvelope . strokeSteps

instance Enveloped StrokeStep where
  getEnvelope = getEnvelope . strokeStepPoint


-- Transformable ---------------------------------------------------------------

instance Transformable Glyph where
  transform = mapGlyphStrokes . transform

instance Transformable Stroke where
  transform = mapStrokeSteps . transform

instance Transformable StrokeStep where
  transform = mapStrokeStepPoint . transform

