{-# LANGUAGE OverloadedStrings #-}

module Parser (
  convertToDiagnostic,
  interpret
) where

import LSPTypes
import Data.Text as T (pack)
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.ParserCombinators.Parsec.Number (int)
import Language.LSP.Types (Diagnostic(..), Range(..), DiagnosticSeverity(..), Position(..))

convertToDiagnostic :: NorminetteMsg -> Diagnostic
convertToDiagnostic nmsg =
  Diagnostic {
    _range = findRange nmsg
  , _severity = Just DsWarning
  , _code = Nothing
  , _source = Just "norminette"
  , _message = pack $ findMessage nmsg
  , _tags = Nothing
  , _relatedInformation = Nothing
  }

findRange :: NorminetteMsg -> Range
findRange (NoLocation _) = Range (Position 0 0) (Position 1 (-1))
findRange (Line l _) = Range (Position (l - 1) 0) (Position l (-1))
findRange (LineColumn (l, c) _) = Range (Position (l - 1) c) (Position l  (-1))

findMessage :: NorminetteMsg -> String
findMessage (NoLocation s) = s
findMessage (Line _ s) = s
findMessage (LineColumn _ s) = s

interpret :: Parsec String () NorminetteMsg
interpret =
  string "Error" *>
  choice [ pNoLocation , pLocation]
  <|> string "Warning: " *> pInvalid

pInvalid :: Parsec String () NorminetteMsg
pInvalid = many (noneOf " ") *>
  (NoLocation . ("file"++) <$> many anyChar)
--Warning: /home/florian/lsp/norminette-lsp/toto.c may not compile or is invalid for some reasons.

pNoLocation :: Parsec String () NorminetteMsg
pNoLocation =
  string ": " *>
    (NoLocation <$> many anyChar)

pLocation :: Parsec String () NorminetteMsg
pLocation = do
  string " (line "
  l <- int
  choice [ pLine l, pLineColumn l]

pLine :: Int -> Parsec String () NorminetteMsg
pLine l =
  string "): " *>
  (Line l <$> many anyChar)

pLineColumn :: Int -> Parsec String () NorminetteMsg
pLineColumn l = do
  string ", col "
  c <- int
  string "): "
  LineColumn (l, c) <$> many anyChar
