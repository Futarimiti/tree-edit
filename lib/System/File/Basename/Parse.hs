module System.File.Basename.Parse (basename) where

import qualified System.FilePath as FilePath
import Text.ParserCombinators.ReadP
import System.File.Basename.Type
import Control.Applicative ((<|>), some)

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
