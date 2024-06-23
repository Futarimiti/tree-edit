module System.File.Tree.Exceptions where

import Control.Exception

newtype IllegalArgument = IllegalArgument String
  deriving (Show, Eq, Ord, Read)

instance Exception IllegalArgument

newtype TypeMismatch = TypeMismatch String
  deriving (Show, Eq, Ord, Read)

instance Exception TypeMismatch
