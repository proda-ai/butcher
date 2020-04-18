{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}


module UI.Butcher.Applicative
  ( -- * Types
    Input(..)
  , CmdParser
  , ParsingError(..)
  , PartialParseInfo(..)
  , CommandDesc
  , PartDesc(..)
  , Visibility(..)
    -- * Run CmdParsers
  , runCmdParserSimpleString
  , runCmdParserSimpleArgv
  , runCmdParser
  , runCmdParserFromDesc
    -- * Building CmdParsers
  , module UI.Butcher.Applicative.Command
  , module UI.Butcher.Applicative.Param
  , module UI.Butcher.Applicative.Flag
    -- * PrettyPrinting CommandDescs (usage/help)
  , module UI.Butcher.Applicative.Pretty
    -- * Wrapper around System.Environment.getArgs
  , module UI.Butcher.Applicative.IO
    -- * Advanced usage
  , emptyCommandDesc
  )
where



#include "prelude.inc"

import qualified Barbies
import qualified Barbies.Bare                  as Barbies
import           Data.Kind
import           Data.List.Extra                ( firstJust )
import           Data.Semigroup                 ( Last(..) )
import           Data.Semigroup.Generic
import           GHC.Generics                   ( Generic )

import           UI.Butcher.Applicative.Command
import           UI.Butcher.Applicative.Flag
import           UI.Butcher.Applicative.IO
import           UI.Butcher.Applicative.Param
import           UI.Butcher.Applicative.Pretty
import           UI.Butcher.Internal.Applicative
import           UI.Butcher.Internal.ApplicativeTypes
import           UI.Butcher.Internal.CommonTypes
import           UI.Butcher.Internal.Interactive



-- runCmdParser
--   :: forall out
--    . Input
--   -> CmdParser out out
--   -> (CommandDesc, Either ParsingError out)
-- runCmdParser initialInput initialParser =
--   let topDesc = toCmdDesc initialParser
--   in  (topDesc, runCmdParserCoreFromDesc initialInput topDesc initialParser)

-- | Run a parser on the given input, and return a struct with all kinds of
-- output. The input does not need to be complete, i.e. if you have a command
-- "clean" then on input "cle" you will not get a successful parse
-- (@_ppi_value@ will be @Left{}@) but other fields will be useful nonetheless.
-- For example @_ppi_inputSugg@ might be "an". Depends on what other commands
-- exist, of course.
--
-- On successful parses, @_ppi_value@ will be @Right{}@ but the other fields
-- still might be useful as well - for example to display the description of
-- the command about to be executed (once user presses enter).
--
-- Note that with haskell's laziness there is no performance cost to
-- using this function - the fields you are not interested in will not be
-- computed.
runCmdParser :: forall out . Input -> CmdParser out out -> PartialParseInfo out
runCmdParser input parser =
  let topDesc = toCmdDesc parser in runCmdParserFromDesc topDesc input parser

-- | This function is the part of the @runCmdParser@ functionality that
-- depends on the input. For interactive use this avoids recomputing the
-- commandDesc.
--
-- For usage see the source of 'runCmdParser'.
runCmdParserFromDesc
  :: forall out
   . CommandDesc
  -> Input
  -> CmdParser out out
  -> PartialParseInfo out
runCmdParserFromDesc topDesc input parser =
  let (localDesc, remainingInput, result) =
        runCmdParserCoreFromDesc input topDesc parser
  in  combinedCompletion input
                         topDesc
                         localDesc
                         remainingInput
                         (fmap Just result)


-- | Wrapper around 'runCmdParser' for very simple usage: Accept a @String@
-- input and return only the output from the parser, or a plain error string
-- on failure.
runCmdParserSimpleString :: String -> CmdParser out out -> Either String out
runCmdParserSimpleString s p =
  let info = runCmdParser (InputString s) p
  in
    case _ppi_value info of
      Left  e           -> Left $ parsingErrorString e
      Right (Just desc) -> Right desc
      Right Nothing ->
        error "Applicative parser should not return Right Nothing"

-- | Wrapper around 'runCmdParser' for very simple usage: Accept a list of
-- @String@s (args)and return only the output from the parser, or a plain
-- error string on failure.
runCmdParserSimpleArgv :: [String] -> CmdParser out out -> Either String out
runCmdParserSimpleArgv s p =
  let info = runCmdParser (InputArgs s) p
  in
    case _ppi_value info of
      Left  e           -> Left $ parsingErrorString e
      Right (Just desc) -> Right desc
      Right Nothing ->
        error "Applicative parser should not return Right Nothing"

-- | Like 'runCmdParser', but with one additional twist: You get access
-- to a knot-tied complete CommandDesc for this full command.
-- runCmdParserWithHelpDesc
--   :: Input -- ^ input to be processed
--   -> (CommandDesc -> CmdParser out out) -- ^ parser to use
--   -> (CommandDesc, Either ParsingError out)
-- runCmdParserWithHelpDesc input cmdF =
--   -- knot-tying at its finest..
--   let (desc, parser) = (toCmdDesc parser, cmdF desc)
--   in  (desc, runCmdParserCoreFromDesc input desc parser)


data MyFlagStruct (c :: Type) (f :: Type -> Type) = MyFlagStruct
  { _userName :: Barbies.Wear c f String
  , _shout    :: Barbies.WearTwo c f Last Bool
  , _dryrun   :: Barbies.WearTwo c f Last Bool
  }
  deriving Generic

instance Barbies.FunctorB (MyFlagStruct Barbies.Covered)
instance Barbies.BareB MyFlagStruct
instance Barbies.TraversableB (MyFlagStruct Barbies.Covered)
instance Semigroup (MyFlagStruct Barbies.Covered Option) where
  (<>) = gmappend

_test :: IO ()
_test = do
  let parser = do
        addCmd "help" $ pure $ do
          putStrLn "help: print helpful help"
        arg :: Int <- addParamRead "SOMEARG" mempty
        -- addCmd "dryrun-arg" $ pure $ do
        --   putStrLn $ "arg = " ++ show arg
        reorderStart
        flags <- traverseBarbie MyFlagStruct
          { _userName = addFlagStringParam "u"
                                           ["user"]
                                           "USERNAME"
                                           (flagDefault "user")
          , _shout    = Last <$> addSimpleBoolFlag "s" ["shout"] mempty
          , _dryrun   = Last <$> addSimpleBoolFlag "d" ["dryrun"] mempty
          }
        reorderStop
        pure $ do
          print arg
          let shoutOrNot = if _shout flags then map Char.toUpper else id
          if (_dryrun flags)
            then do
              putStrLn "would print greeting"
            else do
              putStrLn $ shoutOrNot $ "hello, " ++ _userName flags

  let info = runCmdParser (InputArgs ["42", "--shout", "-u=lsp"]) parser
        -- runCmdParser (InputArgs ["help"]) parser
  let desc = _ppi_mainDesc info
  print desc
  print $ ppHelpDepthOne desc
  case _ppi_value info of
    Left err -> do
      putStrLn "parsing error"
      print err
    Right Nothing  -> putStrLn "no implementation"
    Right (Just f) -> f


-- butcherMain :: ButcherA (IO ()) -> IO ()
-- 
-- type ButcherA out = Writer [ButcherCmd out] ()
-- type ButcherCmd = Ap ButcherCmdF out
-- data ButcherCmdF a
--   = ButcherCmdHelp String (() -> a)
--   | ButcherCmdParamString (String -> a)

