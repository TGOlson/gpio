module Main (main) where

import Control.Monad
import Options.Generic

import System.GPIO

data Command
    = Init Int String
    | Read Int String
    | Write Int String
    | Close Int String
  deriving (Generic, Show)

instance ParseRecord Command

main :: IO ()
main = getRecord "GPIO" >>= \case
    Init x "in"   -> void (initReaderPin (getPin x))
    Init x "out"  -> void (initWriterPin (getPin x))
    Init _ _      -> error "Usage error: use 'in' or 'out' for export direction."

    Read x "in"   -> reattachToReaderPin (getPin x) >>= readPin >>= print
    Read x "out"  -> reattachToWriterPin (getPin x) >>= readPin >>= print
    Read _ _      -> error "Usage error: use 'in' or 'out' for export direction."

    Write x "hi"  -> reattachToWriterPin (getPin x) >>= writePin HI
    Write x "lo"  -> reattachToWriterPin (getPin x) >>= writePin LO
    Write _ _     -> error "Usage error: use 'hi' or 'lo' for write value."

    Close x "in"  -> reattachToReaderPin (getPin x) >>= closePin
    Close x "out" -> reattachToWriterPin (getPin x) >>= closePin
    Close _ _     -> error "Usage error: use 'in' or 'out' for export direction."
  where
    getPin = throwLeft . parsePin


parsePin :: Int -> Either String Pin
parsePin x = case fromInt x of
    Nothing -> Left ("Usage error: could not parse pin value from integer: " ++ show x)
    Just p  -> Right p

throwLeft :: Either String a -> a
throwLeft = either error id
