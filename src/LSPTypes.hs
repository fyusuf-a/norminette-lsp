module LSPTypes (NorminetteMsg(..)) where

data NorminetteMsg = NoLocation String
                   | Line Int String
                   | LineColumn (Int, Int) String
  deriving (Show)
