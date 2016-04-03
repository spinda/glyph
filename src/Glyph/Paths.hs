-- Copyright (C) 2016 Michael Smith.

-- Glyph is bi-licensed under the Lesser GNU General Public License Version 3
-- or later as well as the Mozilla Public License Version 2. You can modify or
-- redistribute its sources under the conditions of these licenses.

-- The text of the Lesser GNU General Public License Version 3 can found in the
-- COPYING.LGPL3 file, and can also be obtained from
-- <http://www.gnu.org/licenses/>.

-- The text of the Mozilla Public License Version 2 can be found in the
-- COPYING.MPL2 file, and can also be obtained from <http://mozilla.org/MPL/2.0/>.

-- | Compute paths to built-in glyph data files.

module Glyph.Paths (
    -- * Built-In Glyph Data File Paths
    getBuiltInGlyphFilePaths
  , makeBuiltInGlyphFilePaths
  ) where

import Data.Bifunctor
import Data.Char

import System.FilePath

import Paths_glyph

--------------------------------------------------------------------------------
-- Built-In Glyph Data File Paths ----------------------------------------------
--------------------------------------------------------------------------------

getBuiltInGlyphFilePaths :: IO [(FilePath, [Char])]
getBuiltInGlyphFilePaths = makeBuiltInGlyphFilePaths <$> getDataDir

makeBuiltInGlyphFilePaths :: FilePath -> [(FilePath, [Char])]
makeBuiltInGlyphFilePaths dataDir =
  map (first ((dataDir </>) . ("glyphs" </>))) all
  where
    all =
      concat [lowerCase, upperCase, punctuation]
    lowerCase =
      map (\c -> (c : ".txt", [c])) ['a'..'z']
    upperCase =
      map (\(f, [c]) -> ("upper_" ++ f, [toUpper c])) lowerCase
    punctuation =
      [ ("colon.txt", ":")
      , ("comma.txt", ",")
      , ("exclamation_mark.txt", "!")
      , ("hyphen.txt", "-")
      , ("period.txt", ".")
      , ("question_mark.txt", "?")
      , ("semicolon.txt", ";")
      , ("upper_comma.txt", "'’")
      ]

