module Main where

import Error
import Monad
import Parser
import Resolver

import Control.Monad.IO.Class
import qualified Data.Text as T
import qualified Data.Text.IO as T
import System.Console.Haskeline
import System.Environment (getArgs)

main :: IO ()
main = do
        args <- getArgs
        case args of
                [] -> repl
                src : _ -> processFile src

repl :: IO ()
repl = runInputT defaultSettings loop
    where
        loop = do
                minput <- getInputLine ">> "
                case minput of
                        Nothing -> outputStrLn "Goodbye."
                        Just "" -> outputStrLn "Goodbye."
                        Just input -> do
                                liftIO $ process (T.pack input)
                                loop

processFile :: String -> IO ()
processFile src = do
        inp <- T.readFile src
        process inp

process :: T.Text -> IO ()
process inp =
        Error.catchError $ do
                (res, st) <- Error.eitherToMonadThrow (Monad.parse inp Parser.parser)
                let opdict = opDict (parser_ust st)
                res' <- resolveFixity opdict res
                print res'