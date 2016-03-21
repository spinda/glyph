import Control.Monad

import Test.Tasty
import Test.Tasty.HUnit

import Longhand.Parse

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parsingTests]

parsingTests :: TestTree
parsingTests = testGroup "Parsing tests"
  [ testCase "Loading the built-in glyph data doesn't crash" $
      void loadBuiltInGlyphData
  ]

