-- | Compute paths to built-in glyph data files.

module Longhand.Glyphs.BuiltIn.Paths (
    -- * Built-In Glyph Data File Paths
    getBuiltInGlyphFilePaths
  , makeBuiltInGlyphFilePaths
  ) where

import Data.Bifunctor
import Data.Char

import System.FilePath

import Paths_longhand

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

