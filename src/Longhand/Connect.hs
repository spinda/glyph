-- | Make connections between glyphs in words.

module Longhand.Connect (
    -- * Make Connections Between Glyphs
  , connectGlyphs
  , connectWord
  , connectLine
  , connectPara
  , connectDoc
  ) where

--------------------------------------------------------------------------------
-- Make Connections Between Glyphs ---------------------------------------------
--------------------------------------------------------------------------------

connectGlyphs :: Located Glyph -> Located Glyph
              -> (Located Glyph, Located Glyph)
connectGlyphs g1 g2 = undefined

connectWord :: GlyphWord -> GlyphWord
connectWord = undefined

connectLine :: GlyphLine -> GlyphLine
connectLine = map connectWord

connectPara :: GlyphPara -> GlyphPara
connectPara = map connectLine

connectDoc :: GlyphDoc -> GlyphDoc
connectDoc = map connectPara

