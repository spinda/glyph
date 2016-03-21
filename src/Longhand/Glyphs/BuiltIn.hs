-- | Built-in base glyph data for handwriting synthesis.
--
-- Provides data for the following glyphs:
--
-- > :,!-.?';’abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ

{-# LANGUAGE TemplateHaskell #-}

module Longhand.Glyphs.BuiltIn (
    -- * Built-In Glyph Data 
    builtInGlyphData
  ) where

import Language.Haskell.TH.Syntax

import System.FilePath

import Longhand.Parse.TH

import Longhand.Glyphs
import Longhand.Glyphs.BuiltIn.Paths

--------------------------------------------------------------------------------
-- Built-In Glyph Data ---------------------------------------------------------
--------------------------------------------------------------------------------

builtInGlyphData :: GlyphMap
builtInGlyphData =
  $(importGlyphFiles =<< (makeBuiltInGlyphFilePaths .
    (</> "../../..") . takeDirectory . loc_filename <$> location))

