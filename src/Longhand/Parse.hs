-- | Parse letter form files in Muse-CGH's text format.

{-# LANGUAGE OverloadedStrings #-}

module Longhand.Parse (
    parseLetterForm
  ) where

import Control.Applicative

import Data.ByteString (ByteString)
import qualified Data.ByteString as B

import Data.Attoparsec.ByteString
import Data.Attoparsec.ByteString.Char8
import Data.Attoparsec.Combinator

import Longhand.Types

parseLetterForm :: ByteString -> Either String Letter
parseLetterForm = parseOnly editingP


editingP :: Parser Letter
editingP = do
  string "Editing"
  parens $ letterP <* comma <* listP (signed decimal)

letterP :: Parser Letter
letterP = do
  string "Letter"
  parens $ do
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
letterSegmentP = do
  string "LetterSeg"
  parens $ do
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
cubicCurveP = do
  string "CubicCurve"
  parens $ do
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
vec2P = do
  string "Vec2"
  parens $ do
    x <- double <* comma
    y <- double
    return (x, y)


boolP :: Parser Bool
boolP = (True  <$ string "true")
    <|> (False <$ string "false")

vectorP :: Parser a -> Parser [a]
vectorP p = string "Vector" *> parens (sepBy p comma)

listP :: Parser a -> Parser [a]
listP p = string "List" *> parens (sepBy p comma)


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

