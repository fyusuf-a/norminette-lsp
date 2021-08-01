import Parser
import qualified Data.Text.Encoding as TE
import qualified Data.Text.IO as TIO
import Text.Parsec
import System.Exit(exitFailure)
import Control.Monad(when)
import System.Environment

main :: IO ()
main = do
  args <- getArgs
  when (length args /= 1) $ do
    putStrLn "Usage: ./norminette-parser file"
    exitFailure
  txt <- TIO.readFile $ head args
  let matches = runParser interpret () (head args) (TE.encodeUtf8 txt)
  case matches of
    Left s -> print s
    Right matches -> do print matches; mapM_ print matches
