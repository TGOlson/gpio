module Main (main) where

import Control.Monad
import Options.Generic
import Safe

import System.GPIO

data Command
    = Init Int String
    | Read Int
    | Write Int Int
    | Close Int String
  deriving (Generic, Show)

instance ParseRecord Command

main :: IO ()
main = getRecord "GPIO" >>= \case
    Init x "in"   -> void (initReaderPin (getPin x))
    Init x "out"  -> void (initWriterPin (getPin x))
    Init _ _      -> error "Usage error: use 'in' or 'out' for export direction."

    Read x        -> reattachToReaderPin (getPin x) >>= readPin >>= print

    Write x 1     -> reattachToWriterPin (getPin x) >>= writePin HI
    Write x 0     -> reattachToWriterPin (getPin x) >>= writePin LO
    Write _ _     -> error "Usage error: use 1 or 0 for write value."

    Close x "in"  -> reattachToReaderPin (getPin x) >>= closePin
    Close x "out" -> reattachToWriterPin (getPin x) >>= closePin
    Close _ _     -> error "Usage error: use 'in' or 'out' for export direction."
  where
    getPin = throwLeft . parsePin

    parsePin :: Int -> Either String Pin
    parsePin x = case readMay ("P" ++ show x) of
        Nothing -> Left ("Usage error: could not parse pin value from integer: " ++ show x)
        Just p  -> Right p

    throwLeft :: Either String a -> a
    throwLeft = either error id
