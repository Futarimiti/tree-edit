module System.File.Tree.Basename.Parse (basenameParseTests) where

import           "tree-edit" System.File.Tree.Basename
import           Text.ParserCombinators.ReadP
import           Test.HUnit

-- Returns @Nothing@ upon fail (ambiguous result or no parse)
extractBasename :: FilePath -> Maybe String
extractBasename path
  | [(Basename bn, "")] <- readP_to_S parser path = Just bn
  | otherwise = Nothing
  where parser = do
          bn <- basename
          eof
          pure bn

basenameParseTests :: Test
basenameParseTests = TestList
  [ TestLabel "Usual cases" usualCases
  , TestLabel "Null input" failOnNullInput
  , TestLabel "Bizzare inputs" bizzareInputs
  ]

failOnNullInput :: Test
failOnNullInput = TestCase $ assertEqual
    "Should pfail on null input"
    Nothing
    (extractBasename "")

usualCases :: Test
usualCases = TestList
  [ TestCase $ Just "abc" @=? (extractBasename "abc")
  , TestCase $ Just "/" @=? (extractBasename "/")
  , TestCase $ Just "def" @=? (extractBasename "/abc/def")
  , TestCase $ Just ".vimrc" @=? (extractBasename "$HOME/.vimrc")
  , TestCase $ Just ".vim" @=? (extractBasename "$HOME/.vim/")
  ]

bizzareInputs :: Test
bizzareInputs = TestList
  [ TestCase $ Just "." @=? (extractBasename ".")
  , TestCase $ Just "a b c d" @=? (extractBasename "???????//////a b c d")
  , TestCase $ Just "%APPDATA%\\ubec\\ " @=? (extractBasename "%APPDATA%\\ubec\\ ")
  , TestCase $ Just " " @=? (extractBasename "%APPDATA%\\ubec/ ")
  , TestCase $ Just "a 123" @=? (extractBasename "/////\\\\\\\\/a 123/////")
  ]
