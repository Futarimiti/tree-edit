module System.File.Tree.Decode.Type where

import System.File.Tree.Type

class DecodeFileTree a where
  decodeTree :: FileTree -> a
