module Main (main) where

import Control.Monad (void)
import Options.Generic

import System.GPIO

data InputCommand
    = Export Int String
    | Unexport Int
    | Read Int
    | Write Int String
  deriving (Generic, Show)

instance ParseRecord InputCommand

-- optparse-generic has trouble with nested types
-- so we can't make 'Command' a parse record
-- Instead we use RawCommand for the generic instance, then
-- convert to the actual 'Command' type.
data Command
    = CmdExport Pin Dir
    | CmdUnexport Pin
    | CmdRead Pin
    | CmdWrite Pin Value
  deriving (Generic, Show)


toCommand :: InputCommand -> Either String Command
toCommand =
    \case Export i "in"  -> (`CmdExport` In)  <$> parsePin i
          Export i "out" -> (`CmdExport` Out) <$> parsePin i
          Export _ _     -> Left "Usage error: use 'in' or 'out' for export direction."
          Unexport i     -> CmdUnexport       <$> parsePin i
          Read i         -> CmdRead           <$> parsePin i
          Write i "hi"   -> (`CmdWrite` HI)   <$> parsePin i
          Write i "lo"   -> (`CmdWrite` LO)   <$> parsePin i
          Write _ _      -> Left "Usage error: use 'hi' or 'lo' for write value."
  where
    parsePin i = case fromInt i of
        Nothing -> Left ("Usage error: could not parse pin value from integer: " ++ show i)
        Just p  -> Right p

main :: IO ()
main =
    toCommand <$> getRecord "GPIO" >>= \case
        Left e    -> error e
        Right cmd -> case cmd of
            CmdExport p In  -> void (initReaderPin p)
            CmdExport p Out -> void (initWriterPin p)
            -- Note: chose a random pin type when closing the pin
            -- the type doesn't actually matter...
            CmdUnexport p   -> closePin (ReaderPin p)
            CmdRead p       -> readPin (ReaderPin p) >>= print
            CmdWrite p v    -> writePin (WriterPin p) v
