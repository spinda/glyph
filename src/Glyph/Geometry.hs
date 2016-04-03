-- Copyright (C) 2016 Michael Smith.

-- Glyph is bi-licensed under the Lesser GNU General Public License Version 3
-- or later as well as the Mozilla Public License Version 2. You can modify or
-- redistribute its sources under the conditions of these licenses.

-- The text of the Lesser GNU General Public License Version 3 can found in the
-- COPYING.LGPL3 file, and can also be obtained from
-- <http://www.gnu.org/licenses/>.

-- The text of the Mozilla Public License Version 2 can be found in the
-- COPYING.MPL2 file, and can also be obtained from <http://mozilla.org/MPL/2.0/>.

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

