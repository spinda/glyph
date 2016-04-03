-- | Built-in base glyph data for handwriting synthesis.
--
-- Provides data for the following glyphs:
--
-- > :,!-.?';’abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ

{-# LANGUAGE TemplateHaskell #-}

module Glyph.BuiltIn (
    -- * Built-In Glyph Data 
    builtInGlyphMap
  ) where

import Language.Haskell.TH.Syntax

import System.FilePath

import Glyph.Paths
import Glyph.TH
import Glyph.Types

--------------------------------------------------------------------------------
-- Built-In Glyph Data ---------------------------------------------------------
--------------------------------------------------------------------------------

builtInGlyphMap :: GlyphMap
builtInGlyphMap =
  $(importGlyphFiles =<< (makeBuiltInGlyphFilePaths .
    (</> "../..") . takeDirectory . loc_filename <$> location))

