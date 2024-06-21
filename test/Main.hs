module Main (main) where

import qualified System.Exit                     as Exit
import           System.File.Tree.Basename.Parse (basenameParseTests)
import           System.File.Tree.IO             (treeCreatingTests)
import           Test.HUnit

main :: IO ()
main = do
  result <- runTestTT tests
  if failures result > 0 || errors result > 0
     then Exit.exitFailure
     else Exit.exitSuccess

tests :: Test
tests = TestList
  [ TestLabel "Basename parsing tests" basenameParseTests
  , TestLabel "Tree creating tests" treeCreatingTests
  ]
