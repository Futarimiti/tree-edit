module System.File.Tree.Basename.Type (Basename (..)) where

-- | The last name from a pathname ignoring any trailing slashes.
-- Does not include the trailing slash for directories.
-- Example: @base.wiki@ for @/home/jsmith/base.wiki@,
-- @jsmith@ for @/home/jsmith/@ or @/home/jsmith@.
--
-- Note: unlike @takeBaseName@ from @filepath@ package
-- (where file extension is stripped), extension will always be kept.
newtype Basename = Basename { getBasename :: FilePath }
  deriving (Show, Read, Eq, Ord)
