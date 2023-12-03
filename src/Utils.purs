module Utils where

import Prelude
import Data.Int (fromString)
import Data.Maybe (isJust)
import Effect (Effect)
import Node.Encoding (Encoding(..))
import Node.FS.Sync (readTextFile)
import Node.Path (FilePath)

readFile :: FilePath -> Effect String
readFile filePath = readTextFile UTF8 filePath


isDigit :: String -> Boolean
isDigit = isJust <<< fromString
