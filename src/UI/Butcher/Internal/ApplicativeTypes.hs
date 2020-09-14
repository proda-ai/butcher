{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
-- {-# LANGUAGE PolyKinds #-}

module UI.Butcher.Internal.ApplicativeTypes
  ( PartDesc(..)
  , EpsilonFlag(..)
  , CmdParser
  , ManyUpperBound(..)
  , Input(..)
  , CommandDesc(..)
  , CmdParserF(..)
  , convertStringToInputParse
  , Visibility(..)
  , CompletionItem(..)
  , ParsingError(..)
  , EnrichedCmdParserF(..)
  , ReorderUnit(..)
  )
where



#include "prelude.inc"
import           Control.Applicative.Free
import qualified Control.Monad.Trans.MultiState.Strict
                                               as MultiStateS
import           Data.STRef

import           Data.Coerce                    ( coerce )
import           GHC.TypeLits                   ( Nat )

import qualified Text.PrettyPrint              as PP

import           UI.Butcher.Internal.CommonTypes
                                               as CommonTypes



data CmdParserF out a
  = CmdParserHelp PP.Doc a
  | CmdParserSynopsis String a
  | CmdParserPeekDesc (CommandDesc -> a)
  | CmdParserPeekInput (String -> a)
  | forall p . Typeable p => CmdParserPartInp
                               PartDesc
                               (Input -> EpsilonFlag -> Maybe (p, Input))
                               (p -> a)
  | forall p . Typeable p => CmdParserPartManyInp
                               ManyUpperBound
                               PartDesc
                               (Input -> EpsilonFlag -> Maybe (p, Input))
                               ([p] -> a)
  | CmdParserChild String Visibility (CmdParser out out) a
  | CmdParserReorderStart                    a
  | CmdParserReorderStop                     a

-- | The CmdParser monad type. It is a free applicative over some functor but
-- users of butcher don't need to know more than that 'CmdParser' is a 'Monad'.
type CmdParser out = Ap (CmdParserF out)

data EnrichedCmdParserF s out a
  = forall p . Typeable p => ViaRef (STRef s (Maybe p)) (p -> a)
  | forall p . Typeable p => ViaRefMany (STRef s [p]) ([p] -> a)
  | Lifted (CmdParserF out a)
  | Final (CmdParser out a)

data ReorderUnit s
  = forall p . ReorderUnit (STRef s (Maybe p))
                           (Input -> EpsilonFlag -> Maybe (p, Input))
  | forall p . ReorderUnitMany ManyUpperBound
                               (STRef s [p])
                               (Input -> EpsilonFlag -> Maybe (p, Input))

convertStringToInputParse
  :: (String -> EpsilonFlag -> (Maybe (p, String)))
  -> (Input -> EpsilonFlag -> Maybe (p, Input))
convertStringToInputParse f i e = case i of
  InputString s -> case f s e of
    Nothing        -> Nothing
    Just (p, rest) -> Just (p, InputString rest)
  input@(InputArgs (a1 : ar)) -> case f a1 e of
    Just (p, "")                -> Just (p, InputArgs ar)
    Just (p, rest) | rest == a1 -> Just (p, input)
    _                           -> Nothing
  InputArgs [] -> case f "" e of
    Just (p, "") -> Just (p, InputArgs [])
    _            -> Nothing
