{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}

module UI.Butcher.Internal.MonadicTypes
  ( CommandDesc(..)
  , cmd_mParent
  , cmd_help
  , cmd_synopsis
  , cmd_parts
  , cmd_children
  , cmd_visibility
  , emptyCommandDesc
  , CmdParserF(..)
  , CmdParser
  , PartDesc(..)
  , Input(..)
  , ParsingError(..)
  , addSuggestion
  , ManyUpperBound(..)
  , Visibility(..)
  , CompletionItem(..)
  , PartParseResult(..)
  , PartParser
  , PartialParseInfo(..)
  , resultFromMaybe
  )
where



#include "prelude.inc"
import           Control.Monad.Free
import qualified Control.Monad.Trans.MultiState.Strict
                                               as MultiStateS

import qualified Lens.Micro.TH                 as LensTH

import qualified Text.PrettyPrint              as PP

import           UI.Butcher.Internal.CommonTypes



data PartParseResult val input
  = Success val input -- value, remaining input
  | Failure (Maybe PartDesc) -- desc of the expected part, if appropriate

type PartParser val input = input -> PartParseResult val input

resultFromMaybe :: Maybe (val, input) -> PartParseResult val input
resultFromMaybe = \case
  Just (x, r) -> Success x r
  Nothing     -> Failure Nothing

data CmdParserF f out a
  =                          CmdParserHelp PP.Doc a
  |                          CmdParserSynopsis String a
  |                          CmdParserPeekDesc (CommandDesc -> a)
  |                          CmdParserPeekInput (String -> a)
  -- TODO: we can clean up this duplication by providing
  -- a function (String -> Maybe (p, String)) -> (Input -> Maybe (p, Input)).
  | forall p . Typeable p => CmdParserPart PartDesc (PartParser p String) (p -> f ()) (p -> a)
  | forall p . Typeable p => CmdParserPartMany ManyUpperBound PartDesc (PartParser p String) (p -> f ()) ([p] -> a)
  | forall p . Typeable p => CmdParserPartInp PartDesc (PartParser p Input) (p -> f ()) (p -> a)
  | forall p . Typeable p => CmdParserPartManyInp ManyUpperBound PartDesc (PartParser p Input) (p -> f ()) ([p] -> a)
  |                          CmdParserChild (Maybe String) Visibility (CmdParser f out ()) (f ()) a
  |                          CmdParserImpl  out                                a
  |                          CmdParserReorderStart                             a
  |                          CmdParserReorderStop                              a
  |                          CmdParserGrouped String                           a
  |                          CmdParserGroupEnd                                 a
  | forall p . Typeable p => CmdParserAlternatives PartDesc [((String -> Bool), CmdParser f out p)] (p -> a)

-- | The CmdParser monad type. It is a free monad over some functor but users
-- of butcher don't need to know more than that 'CmdParser' is a 'Monad'.
type CmdParser f out = Free (CmdParserF f out)


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

deriving instance Functor (CmdParserF f out)

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
