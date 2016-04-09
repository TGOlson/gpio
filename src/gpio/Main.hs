module Main (main) where

import BasicPrelude
import Control.Concurrent

import System.GPIO
import System.GPIO.Types

main :: IO ()
main =
    getArgs >>= \case
        ["export", i, dir] -> case (readMay i >>= fromInt, fromText dir) of
            (Just p, Right d) -> case d of
                In  -> void (initReaderPin p)
                Out -> void (initWriterPin p)
            _ -> usageError
        ["unexport", i] -> case readMay i >>= fromInt of
            -- Note: chose a random pin type when closing the pin - doesn't matter...
            Just p  -> closePin (ReaderPin p)
            Nothing -> usageError
        ["read", i] -> case readMay i >>= fromInt of
            Nothing -> usageError
            Just p  -> readPin (ReaderPin p) >>= print
        ["write", i, val] -> case (readMay i >>= fromInt, fromText val) of
            (Just p, Right v) -> writePin (WriterPin p) v
            _                 -> usageError
        [x] -> case (readMay x :: Maybe Int) of
            Nothing -> usageError
            Just i  -> runTest i
        _ -> error "Illegal usage!"
  where
    usageError = error "gpio (init PIN_NUM DIR|close PIN_NUM|read PIN_NUM|write PIN_NUM VALUE|SECONDS)"
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
