-- Copyright (C) 2016 Michael Smith.

-- Glyph is bi-licensed under the Lesser GNU General Public License Version 3
-- or later as well as the Mozilla Public License Version 2. You can modify or
-- redistribute its sources under the conditions of these licenses.

-- The text of the Lesser GNU General Public License Version 3 can found in the
-- COPYING.LGPL3 file, and can also be obtained from
-- <http://www.gnu.org/licenses/>.

-- The text of the Mozilla Public License Version 2 can be found in the
-- COPYING.MPL2 file, and can also be obtained from <http://mozilla.org/MPL/2.0/>.

-- | Parse glyph data at compile time with Template Haskell.

{-# LANGUAGE TemplateHaskell #-}

module Glyph.TH (
    -- * Template Haskell API
    importGlyphFiles
  , importGlyphFile
  ) where

import Data.Data
import Data.List
import Data.Maybe
import Data.Typeable

import Data.CharMap.Strict (CharMap)
import qualified Data.CharMap.Strict as M

import Language.Haskell.TH.Syntax
import Language.Haskell.TH.Quote

import System.FilePath

import Glyph.Parse
import Glyph.Types

--------------------------------------------------------------------------------
-- Template Haskell API --------------------------------------------------------
--------------------------------------------------------------------------------

importGlyphFiles :: [(FilePath, [Char])] -> Q Exp
importGlyphFiles fs = do
  mapM_ addDependentFile (fst <$> fs)
  result <- runIO $ parseGlyphFiles fs
  case result of
    Left errs -> fail $
      "Glyph failed to parse glyph files:\n\n        "
        ++ intercalate "\n\n        " (describe <$> errs)
    Right out -> dataToExpQ' out
  where
    describe (f, e) = f ++ ":\n            " ++ e

importGlyphFile :: FilePath -> Q Exp
importGlyphFile f = do
  addDependentFile f
  result <- runIO $ parseGlyphFile f
  case result of
    Left err -> fail $
      "Glyph failed to parse glyph file \""
        ++ f ++ "\":\n\n        "
        ++ err
    Right out -> dataToExpQ' out

--------------------------------------------------------------------------------
-- Convert to TH Exp -----------------------------------------------------------
--------------------------------------------------------------------------------

dataToExpQ' :: Data a => a -> Q Exp
dataToExpQ' = dataToExpQ (const Nothing `extQ` glyphMapToExpQ)

glyphMapToExpQ :: GlyphMap -> Maybe (Q Exp)
glyphMapToExpQ m = Just $ do
  items <- mapM dataToExpQ' $ M.toList m
  return $ VarE 'M.fromList `AppE` (ListE items)

extQ :: (Typeable a, Typeable b) => (a -> q) -> (b -> q) -> a -> q
extQ f g a = maybe (f a) g (cast a)

