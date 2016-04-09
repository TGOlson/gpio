module System.GPIO
    -- Re-exported types
    ( Pin(..)
    , ActivePin
    , Value(..)
    , Dir(..)

    -- Exported API
    , initReaderPin
    , initWriterPin
    , readPin
    , writePin
    , closePin
    ) where

import BasicPrelude
import Control.Monad.Trans.Control
import Data.String.Conversions

import System.GPIO.Types


-- Exported API ----------------------------------------------------------------

initReaderPin :: (MonadBaseControl IO m, MonadIO m) => Pin -> m (ActivePin 'In)
initReaderPin p = initPin activePin >> return activePin
  where activePin = ReaderPin p

initWriterPin :: Pin -> IO (ActivePin 'Out)
initWriterPin p = initPin activePin >> return activePin
  where activePin = WriterPin p

readPin :: (MonadBaseControl IO m, MonadIO m) => ActivePin a -> m Value
readPin p = do
    x <- liftIO $ readFile (valuePath $ pin p)

    -- TODO: handle errors after cleaning this up...
    case fromText (runLineHack x) of
        Right v -> return v
        Left e  -> error $ convertString $
            "Error reading value file for \"" <> show p <> "\": " <> e
  where
    -- Note: too lazy to properly handle new lines in the value files
    -- it looks like the gpio interface appends newlines
    -- so file is read as "1\n"
    -- TODO: handle correctly, maybe use hGetChar or something...
    runLineHack t = case lines t of
        [] -> error "Error: runLineHack failed us."
        (x:_) -> x

writePin :: (MonadBaseControl IO m, MonadIO m) => ActivePin 'Out -> Value -> m ()
writePin p v = withVerboseError
    ("Error writing value \"" <> show v <> "\" to " <> show p <> ".")
    $ liftIO (writeFile (valuePath $ pin p) (toText v))

closePin :: (MonadBaseControl IO m, MonadIO m) => ActivePin a -> m ()
closePin p = withVerboseError
    ("Error closing " <> show p <> ". Was this pin already closed?")
    $ liftIO (writeFile unexportPath (pinNumT $ pin p))


-- Internal Pin Utils ----------------------------------------------------------

initPin :: (MonadBaseControl IO m, MonadIO m) => ActivePin a -> m ()
initPin p = do
    let exportErrorMsg = "Error initializing " <> show p <> ". Was this pin already initialized?"
        setDirErrorMsg = "Error setting direction for " <> show p <> "."
    withVerboseError exportErrorMsg export
    withVerboseError setDirErrorMsg setDirection
  where
    export = liftIO $ writeFile exportPath (pinNumT $ pin p)
    setDirection = liftIO $ writeFile (directionPath $ pin p) (toText dir)
    dir :: Dir
    dir = case p of ReaderPin _ -> In
                    WriterPin _ -> Out


withVerboseError :: (MonadBaseControl IO m) => Text -> m () -> m ()
withVerboseError msg = handle handleError
  where
    handleError :: SomeException -> m ()
    handleError e = error $ convertString (msg <> "\nRaw Error: " <> show e)


-- Path Utils ------------------------------------------------------------------

basePath :: FilePath
basePath = "/sys/class/gpio"

exportPath :: FilePath
exportPath = basePath <> "/export"

unexportPath :: FilePath
unexportPath = basePath <> "/unexport"

pinPath :: Pin -> FilePath
pinPath p = basePath <> "/gpio" <> convertString (pinNumT p)

valuePath :: Pin -> FilePath
valuePath p = pinPath p <> "/value"

directionPath :: Pin -> FilePath
directionPath p = pinPath p <> "/direction"
