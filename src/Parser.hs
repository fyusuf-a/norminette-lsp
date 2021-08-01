{-# LANGUAGE OverloadedStrings #-}

module Parser (
  convertToDiagnostic,
  interpret
) where

import LSPTypes
import qualified Data.Text as T
import Text.Parsec.Prim
import Text.Parsec.Combinator
import Text.Parsec.Char
import Text.Parsec.Number
import Language.LSP.Types (Diagnostic(..), Range(..), DiagnosticSeverity(..), Position(..))
import Data.ByteString(ByteString)
import Data.Functor(void)
import Debug.Trace(trace)

convertToDiagnostic :: NorminetteMsg -> Diagnostic
convertToDiagnostic nmsg =
  Diagnostic {
    _range = findRange nmsg
  , _severity = Just DsWarning
  , _code = Nothing
  , _source = Just "norminette"
  , _message = message nmsg
  , _tags = Nothing
  , _relatedInformation = Nothing
  }

findRange :: NorminetteMsg -> Range
findRange (NoLocation _) = Range (Position 0 0) (Position 1 (-1))
findRange (Line l _) = Range (Position (l - 1) 0) (Position l (-1))
findRange (LineColumn _ l c _) = Range (Position (l - 1) c) (Position l  (-1))

interpret :: Parsec ByteString () [NorminetteMsg]
interpret = do
  option () pHeader
  many (noneOf "\n") -- line of the form "filename: Error!" or "filename: OK!"
  newline
  many $ try pLocation <|> pInvalid
  
pHeader :: Parsec ByteString () ()
pHeader = void $ string "Missing or invalid header" *>
                many (noneOf "\n") *> newline

pInvalid :: Parsec ByteString () NorminetteMsg
pInvalid = do
  many1 (noneOf " ")
  NoLocation . ("file"<>) . T.pack <$> many anyChar

pLocation :: Parsec ByteString () NorminetteMsg
pLocation = do
  string "Error:"
  spaces
  errorType <- many (noneOf " \n")
  spaces
  string "(line: "
  spaces
  l <- int
  string ", col:"
  spaces
  c <- int
  string "):\t"
  spaces
  msg <- anyToken `manyTill` (() <$ newline <|> eof)
  return $ LineColumn (T.pack errorType) l c . T.pack $ msg
