module System.File.Tree.Encode.Type where

import System.File.Tree.Type

class EncodeFileTree a where
  encodeTree :: FileTree -> a
