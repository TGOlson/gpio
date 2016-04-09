module Main (main) where

import BasicPrelude
import Control.Concurrent

import System.GPIO
import System.GPIO.Internal

-- Test program, hardcoded to only work with pins 18 and 23
main :: IO ()
main =
    getArgs >>= \case
        -- exporting...
        ["export", "18", "out"] -> void (initWriterPin P18)
        ["export", "18", "in"]  -> void (initReaderPin P18)
        ["export", "23", "out"] -> void (initWriterPin P23)
        ["export", "23", "in"]  -> void (initReaderPin P23)
        -- unexporting...
        ["unexport", "18"]      -> closePin (ReaderPin P18)
        ["unexport", "23"]      -> closePin (ReaderPin P23)
        -- reading...
        ["read", "18"]          -> readPin (ReaderPin P18) >>= print
        ["read", "23"]          -> readPin (ReaderPin P23) >>= print
        -- writing...
        ["write", "18", "1"]    -> writePin (WriterPin P18) HI
        ["write", "18", "0"]    -> writePin (WriterPin P18) LO
        ["write", "23", "1"]    -> writePin (WriterPin P23) HI
        ["write", "23", "0"]    -> writePin (WriterPin P23) LO
        [x]                     -> case (readMay x :: Maybe Int) of
            Nothing -> error "Illegal usage!"
            Just i  -> runTest i
        _ -> error "Illegal usage!"
  where
    runTest s = do
        putStrLn "In program"
        p <- initWriterPin P18

        putStrLn $ "Got pin: " ++ show p
        writePin p HI

        putStrLn "Set to HI"
        v <- readPin p
        putStrLn $ "Pin value: " <> show v

        threadDelay (1000000 * s)
        putStrLn "Delay over"

        writePin p LO
        v' <- readPin p
        putStrLn $ "New Pin value: " <> show v'
        closePin p
