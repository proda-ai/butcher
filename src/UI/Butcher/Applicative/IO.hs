-- | Turn your CmdParser into an IO () to be used as your program @main@.
module UI.Butcher.Applicative.IO
  ( mainFromCmdParser
  -- , mainFromCmdParserWithHelpDesc
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

import           UI.Butcher.Internal.Applicative
import           UI.Butcher.Internal.ApplicativeTypes
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
mainFromCmdParser :: CmdParser (IO ()) (IO ()) -> IO ()
mainFromCmdParser cmd = do
  progName <- System.Environment.getProgName
  args     <- System.Environment.getArgs
  let topDesc = toCmdDesc cmd
  case runCmdParserCoreFromDesc (InputArgs args) topDesc cmd of
    (desc, _remaining, Left err) -> do
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
    (_desc, _remaining, Right out) -> out

putStrErrLn :: String -> IO ()
putStrErrLn s = hPutStrLn stderr s

printErr :: Show a => a -> IO ()
printErr = putStrErrLn . show
