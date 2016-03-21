-- | Parse letter form files in Muse-CGH's text format.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Longhand.Parse (
    -- * Loading Built-In Letter Form Data
    loadBuiltInLetterForms
  , LoadLettersException(..)

    -- * Letter Form File Parsing
  , parseLetterFormFiles
  , parseLetterFormFile
  , parseLetterForm
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
import Longhand.Letters

--------------------------------------------------------------------------------
-- Loading Built-In Letter Form Data -------------------------------------------
--------------------------------------------------------------------------------

data LoadLettersException = LoadLettersException ![(FilePath, String)]
                            deriving (Data, Typeable, Generic)

instance Show LoadLettersException where
  show (LoadLettersException failures) =
    "Failed to load Longhand's built-in letter form data:\n    "
      ++ intercalate "\n    " (describe <$> failures)
    where
      describe (f, e) = f ++ ":\n        " ++ e

instance Exception LoadLettersException where

loadBuiltInLetterForms :: IO LetterMap
loadBuiltInLetterForms = do
  dataDir <- getDataDir
  let lettersDir = dataDir </> "letters"
  let files = map (\(f, cs) -> (lettersDir </> f, cs)) builtInLetterFormFiles
  result <- parseLetterFormFiles files
  case result of
    Left  errs -> throwIO $ LoadLettersException errs
    Right lmap -> return lmap


builtInLetterFormFiles :: [(FilePath, [Char])]
builtInLetterFormFiles = concat [lowerCase, upperCase, punctuation]
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
-- Letter Form File Parsing ----------------------------------------------------
--------------------------------------------------------------------------------

parseLetterFormFiles :: [(FilePath, [Char])]
                     -> IO (Either [(FilePath, String)] LetterMap)
parseLetterFormFiles pairs = do
  results <- mapM parse pairs
  let (errs, letters) = partitionEithers results
  return $ case errs of
    [] -> Right $ M.fromList $ concat letters
    _  -> Left  $ errs
  where
    parse (f, cs) = do
      result <- parseLetterFormFile f
      return $ case result of
        Left  err -> Left (f, err)
        Right out -> Right $ map (\c -> (c, out)) cs


parseLetterFormFile :: FilePath -> IO (Either String Letter)
parseLetterFormFile = fmap parseLetterForm . B.readFile

parseLetterForm :: ByteString -> Either String Letter
parseLetterForm = parseOnly editingP

--------------------------------------------------------------------------------
-- Parse Muse-CGH Letter Text File Format --------------------------------------
--------------------------------------------------------------------------------

editingP :: Parser Letter
editingP = scalaTypeP "Editing" $ letterP <* comma <* listP (signed decimal)

letterP :: Parser Letter
letterP = scalaTypeP "Letter" $ do
  segments <- listP letterSegmentP
  kind     <- option LowerCaseLetter (comma *> letterKindP)
  return $ Letter
    { letterKind     = kind
    , letterSegments = segments
    }

letterKindP :: Parser LetterKind
letterKindP = (UpperCaseLetter <$ (string "Uppercase" <|> string "UpperCase"))
          <|> (LowerCaseLetter <$ (string "Lowercase" <|> string "LowerCase"))
          <|> (PunctuationMark <$ string "PunctuationMark")

letterSegmentP :: Parser LetterSegment
letterSegmentP = scalaTypeP "LetterSeg" $ do
  curve         <- cubicCurveP <* comma
  startWidth    <- double      <* comma
  endWidth      <- double      <* comma
  alignTangent  <- boolP       <* comma
  isStrokeBreak <- boolP
  return $ LetterSegment
    { letterSegmentCurve         = curve
    , letterSegmentStartWidth    = startWidth
    , letterSegmentEndWidth      = endWidth
    , letterSegmentAlignTangent  = alignTangent
    , letterSegmentIsStrokeBreak = isStrokeBreak
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

