module System.GPIO.Types
    ( Pin(..)
    , pinNum
    , fromInt
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
import Safe


-- Available pins and ordering of the pin numbers corresponds to chart found here:
-- https://www.raspberrypi.org/documentation/usage/gpio-plus-and-raspi2/
data Pin
    = P2  | P3  | P4  | P17 | P27 | P22 | P10 | P9  | P11 | P5  | P6  | P13 | P19 | P26
    | P14 | P15 | P18 | P23 | P24 | P25 | P8  | P7  | P12 | P16 | P20 | P21
  deriving (Eq, Read, Show, Enum)

instance ToPath Pin where toPath = show . pinNum
instance ToData Pin where toData = toPath

pinNum :: Pin -> Int
pinNum = read . tail . show

fromInt :: Int -> Maybe Pin
fromInt x = readMay ("P" ++ show x)

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
