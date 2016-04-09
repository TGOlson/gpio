module System.GPIO.Internal where

import BasicPrelude
import Control.Monad.Trans.Control
import Data.String.Conversions


-- Core Types ------------------------------------------------------------------

data Pin = P18 | P23 | P24 | P25 deriving (Show)

pinNum :: Pin -> Int
pinNum = \case P18 -> 18
               P23 -> 23
               P24 -> 24
               P25 -> 25

pinNumT :: Pin -> Text
pinNumT = show . pinNum

data ActivePin (a :: Dir) where
    ReaderPin :: Pin -> ActivePin 'In
    WriterPin :: Pin -> ActivePin 'Out

deriving instance Show (ActivePin a)

pin :: ActivePin a -> Pin
pin = \case ReaderPin p -> p
            WriterPin p -> p

data Dir = In | Out deriving (Show)

instance ToText Dir where
    toText = \case In  -> "in"
                   Out -> "out"
instance FromText Dir where
    fromText = \case "in"   -> Right In
                     "out" -> Right Out
                     x     -> Left ("Cannot parse \"Dir\" from \"" <> x <> "\"")

data Value = HI | LO deriving (Show)

instance ToText Value where
    toText = \case HI -> "1"
                   LO -> "0"
instance FromText Value where
    fromText = \case "1" -> Right HI
                     "0" -> Right LO
                     x   -> Left ("Cannot parse \"Value\" from \"" <> x <> "\"")

-- We are going to do a lot of reading/writing to files w/ custom data types.
-- Create a small interface to keep conversions consistent.
class ToText a where
    toText :: a -> Text

class FromText a where
    fromText :: Text -> Either Text a


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
