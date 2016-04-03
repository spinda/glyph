-- Copyright (C) 2016 Michael Smith.

-- Glyph is bi-licensed under the Lesser GNU General Public License Version 3
-- or later as well as the Mozilla Public License Version 2. You can modify or
-- redistribute its sources under the conditions of these licenses.

-- The text of the Lesser GNU General Public License Version 3 can found in the
-- COPYING.LGPL3 file, and can also be obtained from
-- <http://www.gnu.org/licenses/>.

-- The text of the Mozilla Public License Version 2 can be found in the
-- COPYING.MPL2 file, and can also be obtained from <http://mozilla.org/MPL/2.0/>.

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

