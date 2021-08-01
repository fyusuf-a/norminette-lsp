module LSPTypes (NorminetteMsg(..)) where

import Data.Text

data NorminetteMsg = NoLocation {message :: Text}
                   | Line {line :: Int, message ::  Text}
                   | LineColumn {errorType :: Text, line :: Int, column :: Int, message :: Text}
  deriving (Show)
