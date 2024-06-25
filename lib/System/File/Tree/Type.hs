module System.File.Tree.Type where

import           Control.Exception
import           Data.Aeson
import qualified Data.Aeson           as Value
import qualified Data.Aeson.Key       as Key
import qualified Data.Aeson.KeyMap    as KeyMap
import qualified Data.Aeson.Types     as Aeson
import qualified Data.Text            as Text
import qualified Data.Vector          as Vector
import           GHC.Generics
import           System.File.Basename

data FileTree = File Basename
              | Directory Basename [FileTree]
              deriving (Show, Eq, Ord, Generic)

-- TODO: test suite
instance FromJSON FileTree where
  parseJSON (Value.String text) = case parseBasename (Text.unpack text) of
                                    Left e   -> fail (displayException e)
                                    Right bn -> pure (File bn)
  parseJSON (Value.Object keymap) | [(key, Value.Array vec)] <- KeyMap.toList keymap = do
    let dirName = Key.toString key
    Just bn <- pure $ parseBasename dirName
    subtree <- traverse parseJSON vec
    pure (Directory bn (Vector.toList subtree))
  parseJSON other = Aeson.typeMismatch "single-entry Object or String" other

instance ToJSON FileTree where
  toJSON (File bn) = Value.String (Text.pack (getBasename bn))
  toJSON (Directory bn subtrees) = Value.Object keymap
    where keymap = directoryKey .= contents
          directoryKey = Key.fromString (getBasename bn)
          contents = Value.Array (Vector.fromList (map toJSON subtrees))

