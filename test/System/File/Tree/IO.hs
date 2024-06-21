module System.File.Tree.IO (treeCreatingTests) where

import           Control.Monad.Catch
import           Control.Monad.Logger
import           Data.List                    (sort)
import           Data.Maybe
import           Paths_tree_edit
import           "tree-edit" System.File.Tree
import           Test.HUnit
import           Text.ParserCombinators.ReadP

treeCreatingTests :: Test
treeCreatingTests = TestList
  [ "should fail on non-existing filepaths" ~: nonExistingPaths
  , "should construct correct file tree for test directory" ~: testTreeGeneration
  ]

testTreeGeneration :: Test
testTreeGeneration = TestCase $ do
  testDir <- getDataFileName "test-directory"
  actual <- runStderrLoggingT $ fromFilePath testDir
  let expected = testDirectoryTree
  if | Directory expectedBn expectedSubtrees <- expected
     , Directory actualBn actualSubtrees <- actual
     -> do assertEqual "should get the basename correct" expectedBn actualBn
           assertEqual "should generate correct subtree"
                       (sort expectedSubtrees)
                       (sort actualSubtrees)
     | otherwise -> assertFailure
                      ("look forward a directory but get file: " ++ show actual)

{-
 resources/test-directory
├── dir1
│   └── subfile1.cc
├── dir2.d
├── empty-dir
├── file1.pl
└── file2.very-long-extension
-}
testDirectoryTree :: FileTree
testDirectoryTree = fromJust $ do
  -- basenames
  test_directory <- mkBasename "test-directory"
  dir1 <- mkBasename "dir1"
  subfile1_cc <- mkBasename "subfile1.cc"
  dir2_d <- mkBasename "dir2.d"
  empty_dir <- mkBasename "empty-dir"
  file1_pl <- mkBasename "file1.pl"
  file2_vle <- mkBasename "file2.very-long-extension"
  file3_sml <- mkBasename "file3.sml"
  hidden_file <- mkBasename ".hidden-file"
  hidden_dir <- mkBasename ".hidden-dir"

  pure (Directory test_directory
    [ Directory dir1 [File subfile1_cc]
    , Directory dir2_d [Directory hidden_dir [File file3_sml]]
    , Directory empty_dir []
    , File file1_pl
    , File file2_vle
    , File hidden_file
    ])

nonExistingPaths :: Test
nonExistingPaths = TestCase $ do
  let path = "/NO*****WAY@#$#YOU++===++HAVE---THIS/PATH132123"
  maybe_tree <- createTree path
  Nothing @=? maybe_tree

createTree :: FilePath -> IO (Maybe FileTree)
createTree path = runNoLoggingT $ do
  let handler :: SomeException -> NoLoggingT IO (Maybe FileTree)
      handler _ = pure Nothing
  tree <- catch (Just <$> fromFilePath path) handler
  pure tree

mkBasename :: FilePath -> Maybe Basename
mkBasename path = case readP_to_S parser path of
                    [(bn, "")] -> Just bn
                    _          -> Nothing
                    where parser = do
                            bn <- basename
                            eof
                            pure bn
