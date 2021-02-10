{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}

module Main where

import Parser
import Language.LSP.Types
import Language.LSP.Server
import Language.LSP.Diagnostics
import Control.Monad.IO.Class
import System.Directory (findExecutable, doesFileExist, removeFile)
import System.Environment (getArgs, getEnv, getEnvironment)
import System.Process.Typed (readProcessStdout)
import Data.String (fromString)
import Data.ByteString.Char8 as B (unpack)
import Data.ByteString.Lazy as L (toStrict)
import Data.Either (rights)
import Data.Map as M (insert)
import Data.SortedList as SL (toSortedList)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Control.Exception (catch, IOException)
import Text.Parsec.Prim (runParser)
import Network.URI
import System.IO.Temp (getCanonicalTemporaryDirectory)
import System.Random
import System.FilePath ((</>), takeExtension, dropExtension, takeFileName)
import System.IO

norminette :: FilePath -> IO [Diagnostic]
norminette path = do
  (_, output) <- readProcessStdout (fromString $ "norminette " ++ path)
  let source = tail . lines . B.unpack . L.toStrict $ output
  return . rights . map ((convertToDiagnostic <$>) <$> runParser interpret () "") $ source

writeTempFile :: String   -- file name template
              -> T.Text   -- data to store
              -> IO FilePath
writeTempFile path txt = do
  dir <- getCanonicalTemporaryDirectory
  gen <- newStdGen
  let infiniteList = randomRs ('0','9') gen
  catch (createFile dir infiniteList) $
    let newList = drop 8 infiniteList in
    (const (createFile dir newList):: IOException -> IO [Char])
  where createFile :: FilePath -> [Char] -> IO [Char]
        createFile dir list = do
          let iD = take 8 list
              suffix = takeExtension path
              prefix = dropExtension . takeFileName $ path
              name = dir </> (prefix <> iD <> suffix)
          notProceed <- doesFileExist name
          if notProceed then createFile dir (drop 8 list) else do
            TIO.writeFile name txt
            return name

handlers :: Handlers (LspM ())
handlers = mconcat
  [
    notificationHandler STextDocumentDidOpen $ \_not -> do
      let NotificationMessage _ _ params = _not
          DidOpenTextDocumentParams item = params
          TextDocumentItem (Uri uri) _ version _ = item
      norminetteResult <- liftIO . norminette . getPath $ T.unpack uri
      let diagnosticMap = M.insert Nothing (SL.toSortedList norminetteResult) mempty
      publishDiagnostics maxBound (toNormalizedUri (Uri uri)) (Just version) diagnosticMap ,
    notificationHandler STextDocumentDidChange $ \_not -> do
      let NotificationMessage _ _ params = _not
          DidChangeTextDocumentParams iD (List xs) = params
          TextDocumentContentChangeEvent _ _ text = head xs
          VersionedTextDocumentIdentifier (Uri uri) mVersion = iD
      path <- liftIO $ writeTempFile (getPath $ T.unpack uri) text
      norminetteResult <- liftIO . norminette $ path
      liftIO $ removeFile path
      publishDiagnostics maxBound (toNormalizedUri (Uri uri)) mVersion (partitionBySource norminetteResult) ,
    notificationHandler STextDocumentDidSave $ \_not -> do
      let NotificationMessage _ _ params = _not
          DidSaveTextDocumentParams iD _ = params
          TextDocumentIdentifier (Uri uri) = iD
      norminetteResult <- liftIO . norminette $ getPath (T.unpack uri)
      publishDiagnostics maxBound (toNormalizedUri (Uri uri)) Nothing (partitionBySource norminetteResult)
  ]
    where getPath uri = case parseURI uri of
                          Just uri' -> uriPath uri'
                          _ -> ""

optionsNoSync :: Options
optionsNoSync = defaultOptions {textDocumentSync = Just syncOptions}
  where syncOptions = TextDocumentSyncOptions {_openClose = Just True, _change = Nothing, _willSave = Nothing, _willSaveWaitUntil = Nothing, _save = Just $ InR (SaveOptions $ Just False)}

optionsSync :: Options
optionsSync = defaultOptions {textDocumentSync = Just syncOptions}
  where syncOptions = TextDocumentSyncOptions {_openClose = Just True, _change = Just TdSyncFull, _willSave = Nothing, _willSaveWaitUntil = Nothing, _save = Nothing}

-- norminette-lsp [--sync|--no-sync]
data Sync = Sync
          | NoSync

parseArgs :: [String] -> Either T.Text Sync
parseArgs [x]
  | x == "--sync" = Right Sync
  | x == "--no-sync" = Right NoSync
  | otherwise = Left "norminette-lsp [--sync|--no-sync]"
parseArgs _ = Left "norminette-lsp [--sync|--no-sync]"

main :: IO Int
main = do
    args <- getArgs
    let esync = parseArgs args
    runServer $ ServerDefinition
      { onConfigurationChange = const $ pure $ Right ()
      , doInitialize = \env _req -> do
          case esync of
            Left err -> return (Left (ResponseError ServerErrorStart err Nothing))
            _ -> do
              isNorminette <- findExecutable "norminette"
              case isNorminette of
                Nothing -> return (Left (ResponseError ServerErrorStart "No norminette executable found in $PATH" Nothing))
                _ -> pure $ Right env
      , staticHandlers = handlers
      , interpretHandler = \env -> Iso (runLspT env) liftIO
      , options = case esync of
                    Left _ -> defaultOptions
                    Right Sync -> optionsSync
                    Right NoSync -> optionsNoSync
      }
