module System.File.Basename.Type (Basename (..)) where

import           Data.Aeson
import           GHC.Generics
import qualified Data.Text as Text

-- | The last name from a pathname ignoring any trailing slashes.
-- Does not include the trailing slash for directories.
-- Example: @base.wiki@ for @/home/jsmith/base.wiki@,
-- @jsmith@ for @/home/jsmith/@ or @/home/jsmith@.
--
-- Note: unlike @takeBaseName@ from @filepath@ package
-- (where file extension is stripped), extension will always be kept.
newtype Basename = Basename { getBasename :: FilePath }
  deriving (Show, Read, Eq, Ord) via FilePath
  deriving Generic

-- basename is just a string in json
instance FromJSON Basename where
  parseJSON = withText "Basename" (pure . Basename . Text.unpack)

instance ToJSON Basename where
  toJSON (Basename bn) = String (Text.pack bn)
