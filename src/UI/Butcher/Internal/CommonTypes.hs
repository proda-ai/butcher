{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Butcher.Internal.CommonTypes
  ( CommandDesc (..)
  , cmd_mParent
  , cmd_help
  , cmd_synopsis
  , cmd_parts
  , cmd_hasImpl
  , cmd_children
  , cmd_visibility
  , emptyCommandDesc
  , PartDesc (..)
  , Input (..)
  , EpsilonFlag (..)
  , ParsingError (..)
  , addSuggestion
  , ManyUpperBound (..)
  , Visibility (..)
  , CompletionItem (..)
  , PartialParseInfo (..)
  )
where



#include "prelude.inc"
import           Control.Monad.Free
import qualified Control.Monad.Trans.MultiState.Strict as MultiStateS

import qualified Lens.Micro.TH as LensTH

import qualified Text.PrettyPrint as PP



-- | Butcher supports two input modi: @String@ and @[String]@. Program
-- arguments have the latter form, while parsing interactive command input
-- (e.g. when you implement a terminal of sorts) is easier when you can
-- process the full @String@ without having to wordify it first by some
-- means (and List.words is not the right approach in many situations.)
data Input = InputString String | InputArgs [String]
  deriving (Show, Eq)

data EpsilonFlag = AllowEpsilon | DenyEpsilon deriving Eq

-- | Information about an error that occured when trying to parse some @Input@
-- using some @CmdParser@.
data ParsingError = ParsingError
  { _pe_messages     :: [String]
  , _pe_remaining    :: Input
  , _pe_expectedDesc :: Maybe PartDesc
  }
  deriving (Show)

-- | Specifies whether we accept 0-1 or 0-n for @CmdParserPart@s.
data ManyUpperBound
  = ManyUpperBound1
  | ManyUpperBoundN
  deriving Eq

data Visibility = Visible | Hidden
  deriving (Show, Eq)


-- type CmdParser a = CmdParserM a a

-- data CmdPartParserF a
--   = CmdPartParserHelp String a
--   | forall p . CmdPartParserCore (String -> Maybe (p, String)) -- parser
--                                  (Maybe p) -- optional default value
--                                  (p -> a)
--   | forall p . CmdPartParserOptional (CmdPartParser p)
--                                      (Maybe p -> a)
--   -- the idea here was to allow adding some dynamic data to each "node" of
--   -- the output CommandDesc so the user can potentially add custom additional
--   -- information, and write a custom pretty-printer for e.g. help output
--   -- from that dynamically-enriched CommandDesc structure.
--   -- disabled for now, because i am not sure what exactly "adding to every
--   -- node" involves, because the mapping from Functor to Desc is nontrivial.
--   -- (and because i don't have a direct use-case at the moment..)
--   -- | CmdPartParserCustom Dynamic a
-- 
-- type CmdPartParser = Free CmdPartParserF

---------

-- | A representation/description of a command parser built via the
-- 'CmdParser' monad. Can be transformed into a pretty Doc to display
-- as usage/help via 'UI.Butcher.Monadic.Pretty.ppUsage' and related functions.
--
-- Note that there is the '_cmd_out' accessor that contains @Maybe out@ which
-- might be useful after successful parsing.
data CommandDesc = CommandDesc
  { _cmd_mParent  :: Maybe (Maybe String, CommandDesc)
  , _cmd_synopsis :: Maybe PP.Doc
  , _cmd_help     :: Maybe PP.Doc
  , _cmd_parts    :: [PartDesc]
  , _cmd_hasImpl  :: Bool
  , _cmd_children :: Deque (Maybe String, CommandDesc)
                     -- we don't use a Map here because we'd like to
                     -- retain the order.
  , _cmd_visibility :: Visibility
  }

-- type PartSeqDesc = [PartDesc]

-- | A representation/description of a command's parts, i.e. flags or params.
-- As a butcher user, the higher-level pretty-printing functions for
-- 'CommandDesc' are probably sufficient.
data PartDesc
  = PartLiteral String -- expect a literal string, like "--dry-run"
  | PartVariable String -- expect some user-provided input. The
                               -- string represents the name for the variable
                               -- used in the documentation, e.g. "FILE"
  | PartOptional PartDesc
  | PartAlts [PartDesc]
  | PartSeq [PartDesc]
  | PartDefault String -- default representation
                PartDesc
  | PartSuggestion [CompletionItem] PartDesc
  | PartRedirect String -- name for the redirection
                 PartDesc
  | PartReorder [PartDesc]
  | PartMany PartDesc
  | PartWithHelp PP.Doc PartDesc
  | PartHidden PartDesc
  deriving Show

addSuggestion :: Maybe [CompletionItem] -> PartDesc -> PartDesc
addSuggestion Nothing     = id
addSuggestion (Just sugs) = PartSuggestion sugs


data CompletionItem
  = CompletionString String
  | CompletionDirectory
  | CompletionFile
  deriving Show


{-
command documentation structure
1. terminals. e.g. "--dry-run"
2. non-terminals, e.g. "FILES"
3. sequences, e.g. "<program> FLAGS NUMBER PATH"
-- 4. alternatives, e.g. "--date=(relative|local|iso|rfc|..)"
5. sub-commands: git (init|commit|push|clone|..)
   compared to 4, the subcommands have their own flags and params;
   they essentially "take over".
6. optional, e.g. "cabal run [COMPONENT]"
7. default, e.g. "-O(LEVEL=1)"
8. indirection, e.g. "cabal COMMAND\n\nCOMMAND: ..."
-}

--

-- | Empty 'CommandDesc' value. Mostly for butcher-internal usage.
emptyCommandDesc :: CommandDesc
emptyCommandDesc =
  CommandDesc Nothing Nothing Nothing [] False mempty Visible

instance Show CommandDesc where
  show c = "Command help=" ++ show (_cmd_help c)
        ++ " synopsis=" ++ show (_cmd_synopsis c)
        ++ " mParent=" ++ show (fst <$> _cmd_mParent c)
        ++ " parts.length=" ++ show (length $ _cmd_parts c)
        ++ " parts=" ++ show (_cmd_parts c)
        ++ " children=" ++ show (fst <$> _cmd_children c)

--

data PartialParseInfo out = PartialParseInfo
  { _ppi_mainDesc        :: CommandDesc
  , _ppi_localDesc       :: CommandDesc
  , _ppi_value           :: Either ParsingError (Maybe out)
  , _ppi_line            :: Input
  , _ppi_rest            :: Input
  , _ppi_lastword        :: String
  , _ppi_choices         :: [CompletionItem]
  , _ppi_choicesHelp     :: [(CompletionItem, Maybe String)]
  , _ppi_choiceCommon    :: String
  , _ppi_inputSugg       :: String
  , _ppi_prioDesc        :: Maybe PartDesc
  , _ppi_interactiveHelp :: Int -> PP.Doc
  }

--

LensTH.makeLenses ''CommandDesc
LensTH.makeLenses ''PartDesc

--



-- instance Show FlagDesc where
--   show (FlagDesc _ short long helpM params) = show (short, long, helpM, params) -- TODO: improve

-- class Typeable a => IsParam a where
--   paramParse :: String -> Maybe (a, String, String) -- value, representation, rest
--   paramStaticDef :: a

-- emptyParamDesc :: ParamDesc a
-- emptyParamDesc = ParamDesc Nothing Nothing

-- deriving instance Show a => Show (ParamDesc a)


-- instance Show a => Show (CmdParserF out a) where
--   show (CmdParserHelp s x) = "(CmdParserHelp " ++ show s ++ " " ++ show x ++ ")"
--   show (CmdParserFlag shorts longs _ _) = "(CmdParserFlag -" ++ shorts ++ " " ++ show longs ++ ")"
--   show (CmdParserParam s _ _) = "(CmdParserParam " ++ s ++ ")"
--   show (CmdParserChild s _ _) = "(CmdParserChild " ++ s ++ ")"
--   show (CmdParserRun _) = "CmdParserRun"
