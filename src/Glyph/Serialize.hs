-- | Glyph data (de)serialization. Exposes a simple API for encoding and
-- decoding 'Glyph's and file I/O; use "Data.SafeCopy" or "Data.Serialize" for
-- full functionality.

{-# LANGUAGE TemplateHaskell #-}

module Glyph.Serialize (
    -- * Simple API
    readGlyphFile
  , writeGlyphFile
  , encodeGlyph
  , encodeGlyphLazy
  , decodeGlyph
  , decodeGlyphLazy
  ) where

import Data.SafeCopy
import Data.Serialize

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BL

import Diagrams.Prelude

import Glyph.Types.Internal

--------------------------------------------------------------------------------
-- Simple API ------------------------------------------------------------------
--------------------------------------------------------------------------------

readGlyphFile :: FilePath -> IO (Either String Glyph)
readGlyphFile = (decode <$>) . B.readFile

writeGlyphFile :: FilePath -> Glyph -> IO ()
writeGlyphFile path = B.writeFile path . encode


encodeGlyph :: Glyph -> B.ByteString
encodeGlyph = encode

encodeGlyphLazy :: Glyph -> BL.ByteString
encodeGlyphLazy = encodeLazy

decodeGlyph :: B.ByteString -> Either String Glyph
decodeGlyph = decode

decodeGlyphLazy :: BL.ByteString -> Either String Glyph
decodeGlyphLazy = decodeLazy

--------------------------------------------------------------------------------
-- Data.Serialize Orphan Instances ---------------------------------------------
--------------------------------------------------------------------------------

instance Serialize Glyph where
  get = safeGet
  put = safePut

instance Serialize Stroke where
  get = safeGet
  put = safePut

instance Serialize StrokeStep where
  get = safeGet
  put = safePut

instance Serialize StrokeStepKind where
  get = safeGet
  put = safePut

instance Serialize StrokeCap where
  get = safeGet
  put = safePut

-----------------------------------------------------------------------------------------
-- Data.SafeCopy Orphan Instances -------------------------------------------------------
-----------------------------------------------------------------------------------------

deriveSafeCopy 1 'base ''Glyph
deriveSafeCopy 1 'base ''Stroke
deriveSafeCopy 1 'base ''StrokeStepKind
deriveSafeCopy 1 'base ''StrokeCap

instance SafeCopy StrokeStep where
  putCopy step = contain $ do
    safePut $ unp2 $ strokeStepPoint step
    safePut $ strokeStepWidth step
    safePut $ strokeStepKind step
  getCopy = contain $ do
    point <- uncurry mkP2 <$> safeGet
    width <- safeGet
    kind <- safeGet
    return $ StrokeStep
      { strokeStepPoint = point
      , strokeStepWidth = width
      , strokeStepKind = kind
      }

