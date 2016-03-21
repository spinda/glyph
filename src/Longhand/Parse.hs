-- | Parse glyph files in Muse-CGH's text format.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Longhand.Parse (
    -- * Loading Built-In Glyph Data
    loadBuiltInGlyphData
  , LoadGlyphsException(..)

    -- * Glyph File Parsing
  , parseGlyphFiles
  , parseGlyphFile
  , parseGlyphData
  ) where

import Control.Applicative
import Control.Exception

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator

import Data.Char
import Data.Data
import Data.Either
import Data.List
import Data.Typeable

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.CharMap.Strict (CharMap)
import qualified Data.CharMap.Strict as M

import GHC.Generics

import System.FilePath

import Paths_longhand

import Longhand.Geometry
import Longhand.Glyph

--------------------------------------------------------------------------------
-- Loading Built-In Glyph Data -------------------------------------------------
--------------------------------------------------------------------------------

data LoadGlyphsException = LoadGlyphsException ![(FilePath, String)]
                           deriving (Data, Typeable, Generic)

instance Show LoadGlyphsException where
  show (LoadGlyphsException failures) =
    "Failed to load Longhand's built-in glyph data:\n    "
      ++ intercalate "\n    " (describe <$> failures)
    where
      describe (f, e) = f ++ ":\n        " ++ e

instance Exception LoadGlyphsException where

loadBuiltInGlyphData :: IO GlyphMap
loadBuiltInGlyphData = do
  dataDir <- getDataDir
  let glyphsDir = dataDir </> "glyphs"
  let files = map (\(f, cs) -> (glyphsDir </> f, cs)) builtInGlyphFiles
  result <- parseGlyphFiles files
  case result of
    Left  errs -> throwIO $ LoadGlyphsException errs
    Right lmap -> return lmap


builtInGlyphFiles :: [(FilePath, [Char])]
builtInGlyphFiles = concat [lowerCase, upperCase, punctuation]
  where
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

--------------------------------------------------------------------------------
-- Glyph File Parsing ----------------------------------------------------------
--------------------------------------------------------------------------------

parseGlyphFiles :: [(FilePath, [Char])]
                     -> IO (Either [(FilePath, String)] GlyphMap)
parseGlyphFiles pairs = do
  results <- mapM parse pairs
  let (errs, glyphs) = partitionEithers results
  return $ case errs of
    [] -> Right $ M.fromList $ concat glyphs
    _  -> Left  $ errs
  where
    parse (f, cs) = do
      result <- parseGlyphFile f
      return $ case result of
        Left  err -> Left (f, err)
        Right out -> Right $ map (\c -> (c, out)) cs


parseGlyphFile :: FilePath -> IO (Either String Glyph)
parseGlyphFile = fmap parseGlyphData . B.readFile

parseGlyphData :: ByteString -> Either String Glyph
parseGlyphData = parseOnly editingP

--------------------------------------------------------------------------------
-- Parse Muse-CGH Glyph Text File Format ---------------------------------------
--------------------------------------------------------------------------------

editingP :: Parser Glyph
editingP = scalaTypeP "Editing" $ glyphP <* comma <* listP (signed decimal)

glyphP :: Parser Glyph
glyphP = scalaTypeP "Letter" $ do
  segments <- listP glyphSegmentP
  kind     <- option LowerCaseLetter (comma *> glyphKindP)
  return $ Glyph
    { glyphKind     = kind
    , glyphSegments = segments
    }

glyphKindP :: Parser GlyphKind
glyphKindP = (UpperCaseLetter <$ (string "Uppercase" <|> string "UpperCase"))
         <|> (LowerCaseLetter <$ (string "Lowercase" <|> string "LowerCase"))
         <|> (PunctuationMark <$ string "PunctuationMark")

glyphSegmentP :: Parser GlyphSegment
glyphSegmentP = scalaTypeP "LetterSeg" $ do
  curve         <- cubicCurveP <* comma
  startWidth    <- double      <* comma
  endWidth      <- double      <* comma
  alignTangent  <- boolP       <* comma
  isStrokeBreak <- boolP
  return $ GlyphSegment
    { glyphSegmentCurve         = curve
    , glyphSegmentStartWidth    = startWidth
    , glyphSegmentEndWidth      = endWidth
    , glyphSegmentAlignTangent  = alignTangent
    , glyphSegmentIsStrokeBreak = isStrokeBreak
    }

cubicCurveP :: Parser CubicCurve
cubicCurveP = scalaTypeP "CubicCurve" $ do
  param1 <- vec2P <* comma
  param2 <- vec2P <* comma
  param3 <- vec2P <* comma
  param4 <- vec2P
  return $ CubicCurve
    { cubicCurveParam1 = param1
    , cubicCurveParam2 = param2
    , cubicCurveParam3 = param3
    , cubicCurveParam4 = param4
    }

vec2P :: Parser (Double, Double)
vec2P = scalaTypeP "Vec2" $ do
  x <- double <* comma
  y <- double
  return (x, y)

--------------------------------------------------------------------------------
-- Scala Type Parsers ----------------------------------------------------------
--------------------------------------------------------------------------------

boolP :: Parser Bool
boolP = (True  <$ string "true")
    <|> (False <$ string "false")

listP :: Parser a -> Parser [a]
listP = scalaTypeP' (string "List" <|> string "Vector") . (`sepBy` comma)


scalaTypeP :: ByteString -> Parser a -> Parser a
scalaTypeP = scalaTypeP' . string

scalaTypeP' :: Parser b -> Parser a -> Parser a
scalaTypeP' name args = name *> parens args

--------------------------------------------------------------------------------
-- Misc Parsing Utilities ------------------------------------------------------
--------------------------------------------------------------------------------

between :: Parser a -> Parser b -> Parser c -> Parser c
between start stop inner = start *> inner <* stop

parens :: Parser a -> Parser a
parens = between lparen rparen


lparen :: Parser Char
lparen = char '('

rparen :: Parser Char
rparen = char ')'

comma :: Parser Char
comma = char ',' <* skipMany space

