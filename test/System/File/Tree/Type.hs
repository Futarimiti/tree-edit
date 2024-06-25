{-# LANGUAGE OverloadedLists   #-}
{-# LANGUAGE OverloadedStrings #-}

module System.File.Tree.Type
  ( treeEncodingDecodingTests
  ) where

import           Control.Monad.Logger
import           Data.Aeson
import qualified Data.Aeson                            as Result
import qualified Data.Aeson                            as Value
import qualified Data.Aeson.Key                        as Key
import qualified Data.Aeson.KeyMap                     as KeyMap
import qualified Data.Vector                           as Vector
import           Paths_tree_edit
import           "tree-edit" System.File.Basename.Type
import           "tree-edit" System.File.Tree          as FileTree
import           Test.HUnit
import qualified Data.Text            as Text

treeEncodingDecodingTests :: Test
treeEncodingDecodingTests = TestList
  [ "should perform correct encodings" ~: encodingTests
  , "should perform correct decodings" ~: decodingTests
  ]

decodingTests :: Test
decodingTests = TestList
  [ "should deal with simple cases" ~: simpleDecodings
  , "should deal with real-world example" ~: testExampleDirDecoding
  ]

simpleDecodings :: Test
simpleDecodings = TestList
  [ fileCase "abc.txt"
  , fileCase ".DS_Store"
  , TestCase $ Result.Success (Directory (Basename "empty-dir") [])
      @=? fromJSON (Value.Object (Key.fromString "empty-dir" .= Value.Array Vector.empty))
  ] where fileCase bn = TestCase $ Result.Success (File (Basename bn))
                          @=? fromJSON (Value.String (Text.pack bn))

testExampleDirDecoding :: Test
testExampleDirDecoding = TestCase $ do
  testDir <- getDataFileName "test-directory"
  actualTree <- runNoLoggingT (FileTree.fromFilePath testDir)
  fromJSON testDirectoryTreeJSON @=? Result.Success actualTree

encodingTests :: Test
encodingTests = TestList
  [ "should deal with simple cases" ~: simpleEncodings
  , "should deal with real-world example" ~: testExampleDirEncoding
  ]

simpleEncodings :: Test
simpleEncodings = TestList
  [ TestCase $ Value.String "abc.txt" @=? toJSON (File (Basename "abc.txt"))
  , TestCase $ Value.String ".DS_Store" @=? toJSON (File (Basename ".DS_Store"))
  , TestCase $ Value.Object
                 (KeyMap.singleton
                   (Key.fromString "empty-dir")
                   (Value.Array mempty)) @=? toJSON (Directory (Basename "empty-dir") [])
  , TestCase $ Value.Object
                 (KeyMap.singleton
                   (Key.fromString "directory.d")
                   (Value.Array
                     (Vector.fromList [Value.String "file1", Value.String "file2"])))
               @=? toJSON (Directory
                            (Basename "directory.d")
                            [ File (Basename "file1")
                            , File (Basename "file2")
                            ])
  ]

testExampleDirEncoding :: Test
testExampleDirEncoding = TestCase $ do
  testDir <- getDataFileName "test-directory"
  actualTree <- runNoLoggingT (FileTree.fromFilePath testDir)
  testDirectoryTreeJSON @=? toJSON actualTree

testDirectoryTreeJSON :: Value
testDirectoryTreeJSON = Value.Object
  (KeyMap.fromList
    [("test-directory", Value.Array
      -- well, akshually i copied this from output since who tf would write this by hand
      -- those entries within the array are in a uncertain order,
      -- idk which file gets read first
      -- may break some time later; FIXME
      [ Value.Object (KeyMap.fromList [("empty-dir", Value.Array [])])
      , Value.Object (KeyMap.fromList
                       [("dir2.d", Value.Array
                         [Value.Object
                           (KeyMap.fromList
                             [(".hidden-dir",Value.Array [Value.String "file3.sml"])])])])
      , Value.String "file1.pl"
      , Value.String ".hidden-file"
      , Value.Object (KeyMap.fromList [("dir1",Value.Array [Value.String "subfile1.cc"])])
      , Value.String "file2.very-long-extension"
      ])])
