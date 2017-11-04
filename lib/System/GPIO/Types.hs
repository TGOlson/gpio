module System.GPIO.Types
    ( Pin(..)
    , unpin
    , direction
    , ActivePin(..)
    , Direction(..)
    , Value(..)
    , toPath
    , toData
    , fromData
    ) where


import Data.Monoid ((<>))


data Pin = P18 | P23 | P24 | P25 deriving (Eq, Read, Show, Enum)

instance ToPath Pin where
    toPath = show . pinNum

instance ToData Pin where
    toData = show . pinNum

pinNum :: Pin -> Int
pinNum = \case P18 -> 18
               P23 -> 23
               P24 -> 24
               P25 -> 25


data ActivePin (a :: Direction) where
    ReaderPin :: Pin -> ActivePin 'In
    WriterPin :: Pin -> ActivePin 'Out

deriving instance Show (ActivePin a)

unpin :: ActivePin a -> Pin
unpin = \case ReaderPin p -> p
              WriterPin p -> p

direction :: ActivePin a -> Direction
direction = \case ReaderPin _ -> In
                  WriterPin _ -> Out


data Direction = In | Out deriving (Eq, Show)

instance ToData Direction where
    toData = \case In  -> "in"
                   Out -> "out"

instance FromData Direction where
    fromData = \case "in"  -> Right In
                     "out" -> Right Out
                     x     -> Left ("Cannot parse \"Direction\" from \"" <> x <> "\"")


data Value = HI | LO deriving (Eq, Show)

instance ToData Value where
    toData = \case HI -> "1"
                   LO -> "0"

instance FromData Value where
    fromData = \case "1" -> Right HI
                     "0" -> Right LO
                     x   -> Left ("Cannot parse \"Value\" from \"" <> x <> "\"")


class ToPath a where
    toPath :: a -> FilePath

class ToData a where
    toData :: a -> String

class FromData a where
    fromData :: String -> Either String a
