-- | Turn your CmdParser into an IO () to be used as your program @main@.
module UI.Butcher.Monadic.IO
  ( mainFromCmdParser
  )
where



#include "prelude.inc"
import           Control.Monad.Free
import qualified Control.Monad.Trans.MultiRWS.Strict
                                               as MultiRWSS
import qualified Control.Monad.Trans.MultiState.Strict
                                               as MultiStateS

import qualified Text.PrettyPrint              as PP

import           Data.HList.ContainsType

import           UI.Butcher.Internal.Monadic
import           UI.Butcher.Internal.MonadicTypes
import           UI.Butcher.Monadic.Param
import           UI.Butcher.Monadic.Pretty

import           System.IO



-- | Utility method that allows using a 'CmdParser' as your @main@ function:
--
-- > main = mainFromCmdParser $ do
-- >   addCmdImpl $ putStrLn "This is a fairly boring program."
--
-- Uses @System.Environment.getProgName@ as program name and
-- @System.Environment.getArgs@ as the input to be parsed. Prints some
-- appropriate messages if parsing fails or if the command has no
-- implementation; if all is well executes the \'out\' action (the IO ()).
mainFromCmdParser :: CmdParser Identity (IO ()) () -> IO ()
mainFromCmdParser cmd = do
  progName <- System.Environment.getProgName
  case toCmdDesc (Just progName) cmd of
    Left e -> do
      putStrErrLn
        $ progName
        ++ ": internal error: failed sanity check for butcher main command parser!"
      putStrErrLn $ "(" ++ e ++ ")"
      putStrErrLn $ "aborting."
    Right fullDesc -> do
      args <- System.Environment.getArgs
      case runCmdParserCoreFromDesc fullDesc (InputArgs args) cmd of
        (desc, _, Left err) -> do
          putStrErrLn
            $  progName
            ++ ": error parsing arguments: "
            ++ case _pe_messages err of
                 []      -> ""
                 (m : _) -> m
          putStrErrLn $ case _pe_remaining err of
            InputString ""  -> "at the end of input."
            InputString str -> case show str of
              s | length s < 42 -> "at: " ++ s ++ "."
              s                 -> "at: " ++ take 40 s ++ "..\"."
            InputArgs [] -> "at the end of input"
            InputArgs xs -> case List.unwords $ show <$> xs of
              s | length s < 42 -> "at: " ++ s ++ "."
              s                 -> "at: " ++ take 40 s ++ "..\"."
          putStrErrLn $ "usage:"
          printErr $ ppUsage desc
        (desc, _, Right out) -> case out of
          Nothing -> do
            putStrErrLn $ "usage:"
            printErr $ ppUsage desc
          Just a -> a

putStrErrLn :: String -> IO ()
putStrErrLn s = hPutStrLn stderr s

printErr :: Show a => a -> IO ()
printErr = putStrErrLn . show
