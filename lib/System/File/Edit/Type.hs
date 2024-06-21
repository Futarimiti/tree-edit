module System.File.Edit.Type where

import           Control.Monad.IO.Class
import           System.Process.Typed

newtype ExternalEditor = ExternalEditor
  { process :: FilePath -> ProcessConfig () () () }

class CanEdit editor where
  editFile :: forall io. MonadIO io
           => FilePath
           -> editor
           -> io ()

instance CanEdit ExternalEditor where
  editFile file editor = do
    _code <- runProcess (process editor file)
    pure ()
