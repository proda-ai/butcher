-- | Reexports of everything that is exposed in the submodules.
module UI.Butcher.Monadic
  ( -- * Types
    Input (..)
  , CmdParser
  , ParsingError (..)
  , PartialParseInfo (..)
  , CommandDesc
  , -- * Run or Check CmdParsers
    runCmdParserSimpleString
  , runCmdParser
  , runCmdParserA
  , runCmdParserFromDesc
  , runCmdParserAFromDesc
  , runCmdParserWithHelpDesc
  , toCmdDesc
  , -- * Building CmdParsers
    module UI.Butcher.Monadic.Command
    -- * PrettyPrinting CommandDescs (usage/help)
  , module UI.Butcher.Monadic.Pretty
    -- * Wrapper around System.Environment.getArgs
  , module UI.Butcher.Monadic.IO
  -- , cmds
  -- , sample
  -- , test
  -- , test2
  -- , test3
    -- * Builtin commands
  , addHelpCommand
  , addHelpCommand2
  , addHelpCommandWith
  , addButcherDebugCommand
  , addShellCompletionCommand
  , addShellCompletionCommand'
    -- * Advanced usage
  , mapOut
  , emptyCommandDesc
  , Visibility (..)
  )
where



#include "prelude.inc"

import           UI.Butcher.Internal.Monadic
import           UI.Butcher.Internal.MonadicTypes
import           UI.Butcher.Internal.Interactive
import           UI.Butcher.Monadic.BuiltinCommands
import           UI.Butcher.Monadic.Command
import           UI.Butcher.Monadic.IO
import           UI.Butcher.Monadic.Pretty
import           UI.Butcher.Monadic.Types

import qualified Text.PrettyPrint as PP



#ifdef HLINT
{-# ANN module "HLint: ignore Use import/export shortcut" #-}
#endif



-- | Like 'runCmdParser', but with one additional twist: You get access
-- to a knot-tied complete CommandDesc for this full command. Useful in
-- combination with 'UI.Butcher.Monadic.BuiltinCommands.addHelpCommand'.
--
-- Note that the @CommandDesc@ in the output is _not_ the same value as the
-- parameter passed to the parser function: The output value contains a more
-- "shallow" description. This is more efficient for complex CmdParsers when
-- used interactively, because non-relevant parts of the CmdParser are not
-- traversed unless the parser function argument is forced.
runCmdParserWithHelpDesc
  :: Maybe String -- ^ program name to be used for the top-level @CommandDesc@
  -> Input -- ^ input to be processed
  -> (CommandDesc -> CmdParser Identity out ()) -- ^ parser to use
  -> (CommandDesc, Input, Either ParsingError (Maybe out))
runCmdParserWithHelpDesc mProgName input cmdF =
  let (checkResult, fullDesc)
        -- knot-tying at its finest..
        = ( toCmdDesc mProgName (cmdF fullDesc)
          , either (const emptyCommandDesc) id $ checkResult
          )
  in runCmdParserCoreFromDesc fullDesc input (cmdF fullDesc)


-- | Wrapper around 'runCmdParser' for very simple usage: Accept a @String@
-- input and return only the output from the parser, or a plain error string
-- on failure.
runCmdParserSimpleString :: String -> CmdParser Identity out () -> Either String out
runCmdParserSimpleString s p = case toCmdDesc Nothing p of
  Left err -> Left err
  Right fullDesc -> 
    case runCmdParserCoreFromDesc fullDesc (InputString s) p of
      (_, _, Left e) -> Left $ parsingErrorString e
      (_, _, Right outM) ->
        maybe (Left "command has no implementation") Right $ outM


-- | Runs a 'CmdParser' on the given 'Input', returning the 'PartialParseInfo'
-- struct that encodes both general success/failure and that has additional
-- fields that are useful for interactive help or feedback to the user
-- (think something like "did you mean to use command foo?").
runCmdParser
  :: forall out
   . Maybe String -- ^ top-level command name
  -> Input
  -> CmdParser Identity out ()
  -> PartialParseInfo out
runCmdParser mTopLevel input parser =
  let topDesc = case toCmdDesc mTopLevel parser of
        Left  err -> error err
        Right d   -> d
  in  runCmdParserFromDesc topDesc input parser

-- | Runs a parser given 'Input', a 'CmdParser' and the 'CommandDesc' that was
-- derived from the 'CmdParser' using 'toCmdDesc'.
-- 'runCmdParser' will do both steps, but this is useful
-- a) if the 'CommandDesc' can be re-used because the 'Input' changes but the
-- 'CmdParser' does not.
-- b) because in some (rare) cases 'toCmdDesc' may fail, and calling it
-- explicitly allows handling that case properly.
runCmdParserFromDesc
  :: forall out
   . CommandDesc
  -> Input
  -> CmdParser Identity out ()
  -> PartialParseInfo out
runCmdParserFromDesc topDesc input parser =
  let (localDesc, remainingInput, result) =
        runCmdParserCoreFromDesc topDesc input parser
  in  combinedCompletion input topDesc localDesc remainingInput result

-- | The Applicative-enabled version of 'runCmdParser'.
runCmdParserA
  :: forall f out
   . Applicative f
  => Maybe String -- ^ top-level command name
  -> Input
  -> CmdParser f out ()
  -> f (PartialParseInfo out)
runCmdParserA mTopLevel input parser =
  let topDesc = case toCmdDesc mTopLevel parser of
        Left  err -> error err
        Right d   -> d
  in  runCmdParserAFromDesc topDesc input parser

-- | The Applicative-enabled version of 'runCmdParserA'.
runCmdParserAFromDesc
  :: forall f out
   . Applicative f
  => CommandDesc
  -> Input
  -> CmdParser f out ()
  -> f (PartialParseInfo out)
runCmdParserAFromDesc topDesc input parser =
  let mapper (localDesc, remainingInput, result) =
        combinedCompletion input topDesc localDesc remainingInput result
  in  mapper <$> runCmdParserCoreFromDescA topDesc input parser

--------------------------------------
-- all below is for testing purposes
--------------------------------------


_cmds :: CmdParser Identity (IO ()) ()
_cmds = do
  addCmd "echo" $ do
    addCmdHelpStr "print its parameter to output"
    str <- addParamRead "STRING" (paramHelpStr "the string to print")
    addCmdImpl $ do
      putStrLn str
  addCmd "hello" $ do
    addCmdHelpStr "greet the user"
    reorderStart
    short <- addSimpleBoolFlag "" ["short"] mempty
    name <- addParamRead "NAME" (paramHelpStr "your name, so you can be greeted properly"
                              <> paramDefault "user")
    reorderStop
    addCmdImpl $ do
      if short
        then putStrLn $ "hi, " ++ name ++ "!"
        else putStrLn $ "hello, " ++ name ++ ", welcome from butcher!"
  addCmd "foo" $ do
    addCmdHelpStr "foo"
    desc <- peekCmdDesc
    addCmdImpl $ do
      putStrLn "foo"
      print $ ppHelpShallow desc
  addCmd "help" $ do
    desc <- peekCmdDesc
    addCmdImpl $ do
      print $ ppHelpShallow $ maybe undefined snd (_cmd_mParent desc)

data Sample = Sample
  { _hello :: Int
  , _s1   :: String
  , _s2   :: String
  , _quiet :: Bool
  }
  deriving Show

-- sample :: OPA.Parser Sample
-- sample = Sample
--      <$> OPA.option OPA.auto
--          ( OPA.long "hello"
--         <> OPA.metavar "TARGET"
--         <> OPA.help "Target for the greeting" )
--      <*> OPA.strArgument (OPA.metavar "S1")
--      <*> OPA.strArgument (OPA.metavar "S2")
--      <*> OPA.switch
--          ( OPA.long "quiet"
--         <> OPA.help "Whether to be quiet" )
-- 
-- test :: String -> OPA.ParserResult Sample
-- test s = OPA.execParserPure OPA.defaultPrefs (OPA.ParserInfo sample True mempty mempty mempty (-13) True) (List.words s)

_test2 :: IO ()
_test2 = case toCmdDesc (Just "butcher") _cmds of
  Left e -> putStrLn $ "LEFT: " ++ e
  Right desc -> do
    print $ ppUsage desc
    print $ maybe undefined id $ ppUsageAt ["hello"] desc

_test3 :: String -> IO ()
_test3 s = do
  case _ppi_value info of
    Left err -> do
      print err
      print $ ppHelpShallow (_ppi_localDesc info)
      _cmd_mParent (_ppi_localDesc info) `forM_` \(_, d) -> do
        print $ ppUsage d
    Right Nothing -> do
      putStrLn "command is missing implementation!"
      print $ ppHelpShallow (_ppi_localDesc info)
    Right (Just f) -> f
 where
  info = runCmdParser Nothing (InputString s) _cmds
