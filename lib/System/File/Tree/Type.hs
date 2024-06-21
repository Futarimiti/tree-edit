module System.File.Tree.Type where

import           System.File.Tree.Basename

data FileTree = File Basename
              | Directory Basename [FileTree]
              deriving (Show, Eq, Ord)
