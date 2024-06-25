module System.File.Tree.Decode
  ( DecodeFileTree (..)
  ) where

import           Control.Monad.Catch
import           Data.Aeson
import           System.File.Tree.Exceptions
import           System.File.Tree.Type

class DecodeFileTree a where
  decodeTree :: MonadThrow m => a -> m FileTree
  -- default: a -> aeson value -> tree
  default decodeTree :: (ToJSON a, MonadThrow m) => a -> m FileTree
  decodeTree a = case fromJSON (toJSON a) of
                   Success tree -> pure tree
                   Error reason -> throwM (IllegalArgument reason)

instance DecodeFileTree FileTree where
  decodeTree = return
