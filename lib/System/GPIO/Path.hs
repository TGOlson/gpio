module System.GPIO.Path
    ( basePath
    , exportPath
    , unexportPath
    , pinPath
    , valuePath
    , directionPath
    ) where

import Data.Monoid       ((<>))

import System.GPIO.Types


basePath :: FilePath
basePath = "/sys/class/gpio"

exportPath :: FilePath
exportPath = basePath <> "/export"

unexportPath :: FilePath
unexportPath = basePath <> "/unexport"

pinPath :: Pin -> FilePath
pinPath pin = basePath <> "/gpio" <> toPath pin

valuePath :: Pin -> FilePath
valuePath pin = pinPath pin <> "/value"

directionPath :: Pin -> FilePath
directionPath pin = pinPath pin <> "/direction"
