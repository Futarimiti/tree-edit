module System.File.Tree.Type where

import           System.File.Basename

data FileTree = File Basename
              | Directory Basename [FileTree]
              deriving (Show, Eq, Ord)
