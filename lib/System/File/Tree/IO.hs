{-# LANGUAGE OverloadedStrings #-}
module System.File.Tree.IO (fromFilePath) where

import           Control.Monad
import           Control.Monad.Catch
import           Control.Monad.IO.Class
import           Control.Monad.Logger
import           System.Directory
import           System.File.Basename
import           System.File.Tree.Type
import           System.FilePath
import           System.IO.Error

fromFilePath :: (MonadIO m, MonadLogger m, MonadThrow m)
             => FilePath
             -> m FileTree
fromFilePath path = do
  $logDebug "Current path:"
  $logDebugSH path
  exists <- liftIO (doesPathExist path)
  when (not exists) $ do
    $logError "Given path does not exist, or its parent directory cannot accessible."
    throwM (mkIOError doesNotExistErrorType "" Nothing (Just path))
  base <- parseBasename path
  isDir <- liftIO (doesDirectoryExist path)
  tree <- if isDir then do $logDebug "Path points to a directory"
                           subEntriesBasename <- liftIO (listDirectory path)
                           $logDebug "Found sub entries:"
                           $logDebugSH subEntriesBasename
                           let subEntriesFullname = map (path </>) subEntriesBasename
                           subEntries <- traverse fromFilePath subEntriesFullname
                           pure (Directory base subEntries)
                   else do $logDebug "Path points to a file"
                           pure (File base)
  pure tree
