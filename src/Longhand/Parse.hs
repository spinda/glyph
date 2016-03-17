-- | Parse letter form files in Muse-CGH's text format.

{-# LANGUAGE OverloadedStrings #-}

module Longhand.Parse (
    -- * Letter Form File Parsing
    parseLetterFormFile
  , parseLetterForm
  ) where

import Control.Applicative

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator

import Longhand.Types

--------------------------------------------------------------------------------
-- Letter Form File Parsing ----------------------------------------------------
--------------------------------------------------------------------------------

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
  segments <- vectorP letterSegmentP <* comma
  kind     <- letterKindP
  return $ Letter
    { letterKind     = kind
    , letterSegments = segments
    }

letterKindP :: Parser LetterKind
letterKindP = (UpperCaseLetter <$ string "Uppercase")
          <|> (LowerCaseLetter <$ string "Lowercase")
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

vectorP :: Parser a -> Parser [a]
vectorP = scalaListTypeP "Vector"

listP :: Parser a -> Parser [a]
listP = scalaListTypeP "List"

--------------------------------------------------------------------------------
-- Scala Type Parsing Tools ----------------------------------------------------
--------------------------------------------------------------------------------

scalaTypeP :: ByteString -> Parser a -> Parser a
scalaTypeP name args = string name *> parens args

scalaListTypeP :: ByteString -> Parser a -> Parser [a]
scalaListTypeP name item = scalaTypeP name $ sepBy item comma

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

