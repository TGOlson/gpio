module System.GPIO.Types
    ( Pin(..)
    , pinNum
    , pinNumT
    , fromInt
    , ActivePin(..)
    , pin
    , Dir(..)
    , Value(..)
    , fromText
    , toText
    ) where

import BasicPrelude
import Data.String.Conversions


-- Core Types ------------------------------------------------------------------

data Pin = P18 | P23 | P24 | P25 deriving (Eq, Show, Enum)

pinNumDict :: [(Pin, Int)]
pinNumDict = [ (P18, 18)
             , (P23, 23)
             , (P24, 24)
             , (P25, 25)
             ]

pinNum :: Pin -> Int
pinNum p = fromMaybe unexpectedError (lookup p pinNumDict)
  where
    unexpectedError = error $ convertString
        ("Unexpected error. " ++ show p ++ " not defined in 'pinNumDict'")

pinNumT :: Pin -> Text
pinNumT = show . pinNum

fromInt :: Int -> Maybe Pin
fromInt i = lookup i (swap <$> pinNumDict)

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
    fromText = \case "in"  -> Right In
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
