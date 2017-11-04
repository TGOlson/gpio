module System.GPIO
    -- Re-exported types
    ( Pin(..)
    , ActivePin
    , Value(..)
    , Direction(..)

    -- Exported API
    , initReaderPin
    , initWriterPin
    , readPin
    , writePin
    , reattachToReaderPin
    , reattachToWriterPin
    , closePin
    ) where

import Control.Exception      (SomeException (..))
import Control.Monad
import Control.Monad.Catch
import Control.Monad.IO.Class
import Data.Monoid            ((<>))
import System.Directory

import System.GPIO.Types


initReaderPin :: (MonadCatch m, MonadIO m) => Pin -> m (ActivePin 'In)
initReaderPin pin = let activePin = ReaderPin pin
    in initPin activePin >> return activePin

initWriterPin :: (MonadCatch m, MonadIO m) => Pin -> m (ActivePin 'Out)
initWriterPin pin = let activePin = WriterPin pin
    in initPin activePin >> return activePin

initPin :: (MonadCatch m, MonadIO m) => ActivePin a -> m ()
initPin pin = do
    withVerboseError (InitPinException (unpin pin)) $
        writeFileM exportPath (toData $ unpin pin)

    withVerboseError (SetDirectionException (unpin pin) (direction pin)) $
        writeFileM (directionPath $ unpin pin) (toData (direction pin))


readPin :: (MonadCatch m, MonadIO m) => ActivePin a -> m Value
readPin pin = do
    x <- readFileM $ valuePath (unpin pin)

    -- TODO: handle errors after cleaning this up...
    case fromData (runLineHack x) of
        Right v -> return v
        Left e  -> throwM $ ReadPinException (unpin pin) e
  where
    -- Note: too lazy to properly handle new lines in the value files
    -- it looks like the gpio interface appends newlines
    -- so file is read as "1\n"
    -- TODO: handle correctly, maybe use hGetChar or something...
    runLineHack t = case lines t of
        []    -> error "Error: runLineHack failed us."
        (x:_) -> x


writePin :: (MonadCatch m, MonadIO m) => Value -> ActivePin 'Out -> m ()
writePin value pin = withVerboseError (WritePinException (unpin pin) value)
    $ writeFileM (valuePath $ unpin pin) (toData value)

-- Get an active pin from a pin, preserving the invariants required when a pin is initialized.
-- Useful for CLI type commands where pointers to an active pin can be lost between calls.
reattachToReaderPin :: (MonadCatch m, MonadIO m) => Pin -> m (ActivePin 'In)
reattachToReaderPin = reattachToPin . ReaderPin

reattachToWriterPin :: (MonadCatch m, MonadIO m) => Pin -> m (ActivePin 'Out)
reattachToWriterPin = reattachToPin . WriterPin

reattachToPin :: (MonadCatch m, MonadIO m) => ActivePin a -> m (ActivePin a)
reattachToPin pin = do
    let err = ReattachPinException (unpin pin)

    exists <- liftIO $ doesFileExist (directionPath (unpin pin))
    unless exists $ throwM (err "Pin was never initialized")

    v <- fromData <$> readFileM (directionPath (unpin pin))
    dir <- either (throwM . err) return v

    unless (dir == direction pin) $ throwM (err "Attempting to reattach to pin in wrong direction")

    return pin


closePin :: (MonadCatch m, MonadIO m) => ActivePin a -> m ()
closePin pin = withVerboseError (ClosePinException (unpin pin))
    $ writeFileM unexportPath (toData $ unpin pin)

-- Internal --------------------------------------------------------------------------------------------------

data PinException
    = InitPinException Pin String
    | SetDirectionException Pin Direction String
    | ReadPinException Pin String
    | WritePinException Pin Value String
    | ReattachPinException Pin String
    | ClosePinException Pin String
  deriving (Show)

instance Exception PinException




withVerboseError :: MonadCatch m => (String -> PinException) -> m () -> m ()
withVerboseError pinException = handle $ \(e :: SomeException) -> throwM $ pinException (show e)

writeFileM :: MonadIO m => FilePath -> String -> m ()
writeFileM fp = liftIO . writeFile fp

readFileM :: MonadIO m => FilePath -> m String
readFileM = liftIO . readFile

-- Path Utils ------------------------------------------------------------------------------------------------

basePath :: FilePath
basePath = "/sys/class/gpio"

exportPath :: FilePath
exportPath = basePath <> "/export"

unexportPath :: FilePath
unexportPath = basePath <> "/unexport"

pinPath :: Pin -> FilePath
pinPath pin = basePath <> "/gpio" <> toPath pin

valuePath :: Pin -> FilePath
valuePath pin = pinPath pin <> "/value"

directionPath :: Pin -> FilePath
directionPath pin = pinPath pin <> "/direction"
