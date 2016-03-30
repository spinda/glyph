-- | Connecting glyphs to form words (called "chunks", because 'Word' is
-- already a thing).

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}

module Longhand.Chunks (
    -- * Strings of Connected Glyphs
    Chunk(..)
  , connectGlyphs
  , mapChunkGlyphs
  ) where

import Data.Data
import Data.List
import Data.Typeable

import Diagrams.Prelude

import GHC.Generics

import Longhand.Glyphs

--------------------------------------------------------------------------------
-- Strings of Connected Glyphs -------------------------------------------------
--------------------------------------------------------------------------------

newtype Chunk = Chunk { chunkGlyphs :: [Glyph] }
               deriving (Eq, Show, Data, Typeable, Generic)


connectGlyphs :: [Glyph] -> Chunk
connectGlyphs = Chunk . reverse . fst . foldl' (uncurry layoutGlyph) ([], 0) 

-- TODO: Add connections between glyphs
-- TODO: Track and line up glyph baselines (with jitter in perturbation step)
layoutGlyph :: [Glyph] -> Double -> Glyph -> ([Glyph], Double)
layoutGlyph gs x g = (g' : gs, x + width)
  where
    g' = translate delta g
    delta = V2 (x - minX) 0
    width = maxX - minX
    minX = fst $ unp2 $ envelopeP unit_X envelope
    maxX = fst $ unp2 $ envelopeP unitX envelope
    envelope = getEnvelope g


mapChunkGlyphs :: (Glyph -> Glyph) -> Chunk -> Chunk
mapChunkGlyphs f c = c { chunkGlyphs = f <$> chunkGlyphs c }


type instance V Chunk = V2
type instance N Chunk = Double

instance Transformable Chunk where
  transform = mapChunkGlyphs . transform

instance Enveloped Chunk where
  getEnvelope = mconcat . map getEnvelope . chunkGlyphs

