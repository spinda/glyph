-- | Parse glyph data at compile time with Template Haskell.

{-# LANGUAGE TemplateHaskell #-}

module Longhand.TH (
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

import Longhand.Parse
import Longhand.Types

--------------------------------------------------------------------------------
-- Template Haskell API --------------------------------------------------------
--------------------------------------------------------------------------------

importGlyphFiles :: [(FilePath, [Char])] -> Q Exp
importGlyphFiles fs = do
  mapM_ addDependentFile (fst <$> fs)
  result <- runIO $ parseGlyphFiles fs
  case result of
    Left errs -> fail $
      "Longhand failed to parse glyph files:\n\n        "
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
      "Longhand failed to parse glyph file \""
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

