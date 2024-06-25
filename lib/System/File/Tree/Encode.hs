module System.File.Tree.Encode
  ( EncodeFileTree (..)
  ) where

import           Control.Monad.Catch
import           Data.Aeson
import qualified Data.Aeson                    as Result
import qualified Data.Aeson                    as Value
import           System.File.Tree.Exceptions
import           System.File.Tree.Type

class EncodeFileTree a where
  encodeTree :: MonadThrow m => FileTree -> m a
  -- default: tree -> aeson value -> a
  default encodeTree :: (FromJSON a, MonadThrow m) => FileTree -> m a
  encodeTree tree = case Value.fromJSON (toJSON tree) of
                      Result.Success a    -> pure a
                      Result.Error reason -> throwM (TypeMismatch reason)

instance EncodeFileTree FileTree where
  encodeTree = return
