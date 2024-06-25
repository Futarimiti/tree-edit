module System.File.Basename.Parse
  ( basename
  , parseBasename
  ) where

import           Control.Applicative          (some, (<|>))
import           Control.Monad.Catch
import           System.File.Basename.Type
import           System.File.Tree.Exceptions
import qualified System.FilePath              as FilePath
import           Text.ParserCombinators.ReadP

parseBasename :: MonadThrow m => String -> m Basename
parseBasename s = case readP_to_S parser s of
                    [(bn, "")] -> return bn
                    [] -> throwM (IllegalArgument $ "Cannot parse basename of '" ++ s ++ "' (0 results)")
                    _ -> throwM (IllegalArgument $ "Cannot parse basename of '" ++ s ++ "' (ambiguous results)")
  where parser = do
          bn <- basename
          eof
          pure bn


-- | Extract the basename of the given file/directory.
--
-- >>> readP_to_S basename ""
-- []
-- >>> readP_to_S basename "/"
-- [("/", "")]
-- >>> readP_to_S basename "~/.vimrc"
-- [(".vimrc", "")]
basename :: ReadP Basename
basename = root <|> do
  optional parentDirectory
  filepath <- some notSeparator
  _ <- many separator
  pure (Basename filepath)

root :: ReadP Basename
root = do
  sep <- separator
  _ <- many separator
  pure (Basename [sep])

parentDirectory :: ReadP FilePath
parentDirectory = do
  dirName <- many get
  _ <- separator
  pure dirName

-- A path separator.
separator :: ReadP Char
separator = satisfy FilePath.isPathSeparator

notSeparator :: ReadP Char
notSeparator = satisfy (not . FilePath.isPathSeparator)
