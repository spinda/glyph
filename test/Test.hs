import Control.Monad

import Data.CharMap.Strict (CharMap)
import qualified Data.CharMap.Strict as M

import Test.Tasty
import Test.Tasty.HUnit

import Longhand.Glyphs.BuiltIn
import Longhand.Glyphs.BuiltIn.Paths
import Longhand.Parse

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parsingTests]


parsingTests :: TestTree
parsingTests = testGroup "Parsing tests"
  [ builtInGlyphsParseTest
  ]

builtInGlyphsParseTest :: TestTree
builtInGlyphsParseTest =
  testCase "The built-in glyph data parses successfully" $ do
    files <- getBuiltInGlyphFilePaths
    result <- parseGlyphFiles files
    case result of
      Left _ -> assertFailure "Glyph data had parse errors"
      Right parsed ->
        zipWithM_ checkGlyphData (M.toList parsed)
                                 (M.toList builtInGlyphData)
  where
    checkGlyphData (c1, l1) (c2, l2) = do
      assertEqual ("Glyph charaters don't match") c1 c2
      assertEqual ("Glyph data for '" ++ c1 : "' doesn't match") l1 l2

