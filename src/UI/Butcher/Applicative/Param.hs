{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module UI.Butcher.Applicative.Param
  ( Param(..)
  , paramHelp
  , paramHelpStr
  , paramDefault
  , paramSuggestions
  , paramFile
  , paramDirectory
  , addParamRead
  , addParamReadOpt
  , addParamString
  , addParamStringOpt
  , addParamStrings
  , addParamNoFlagString
  , addParamNoFlagStringOpt
  , addParamNoFlagStrings
  , addParamRestOfInput
  , addParamRestOfInputRaw
  )
where



#include "prelude.inc"

import           Control.Applicative.Free
import           Control.Monad.ST
import           Data.Kind
import           Data.List.Extra                ( firstJust )
import           Data.STRef
import qualified Text.PrettyPrint              as PP

import           UI.Butcher.Internal.ApplicativeTypes
import           UI.Butcher.Internal.Applicative
import           UI.Butcher.Internal.Pretty


data Param p = Param
  { _param_default     :: Maybe p
  , _param_help        :: Maybe PP.Doc
  , _param_suggestions :: Maybe [CompletionItem]
  }

appendParam :: Param p -> Param p -> Param p
appendParam (Param a1 b1 c1) (Param a2 b2 c2) =
  Param (a1 <|> a2) (b1 <> b2) (c1 <> c2)

instance Semigroup (Param p) where
  (<>) = appendParam

instance Monoid (Param p) where
  mempty  = Param Nothing Nothing Nothing
  mappend = (<>)

-- | Create a 'Param' with just a help text.
paramHelpStr :: String -> Param p
paramHelpStr s = mempty { _param_help = Just $ PP.text s }

-- | Create a 'Param' with just a help text.
paramHelp :: PP.Doc -> Param p
paramHelp h = mempty { _param_help = Just h }

-- | Create a 'Param' with just a default value.
paramDefault :: p -> Param p
paramDefault d = mempty { _param_default = Just d }

-- | Create a 'Param' with just a list of suggestion values.
paramSuggestions :: [String] -> Param p
paramSuggestions ss =
  mempty { _param_suggestions = Just $ CompletionString <$> ss }

-- | Create a 'Param' that is a file path.
paramFile :: Param p
paramFile = mempty { _param_suggestions = Just [CompletionFile] }

-- | Create a 'Param' that is a directory path.
paramDirectory :: Param p
paramDirectory = mempty { _param_suggestions = Just [CompletionDirectory] }


-- | Add a parameter to the 'CmdParser' by making use of a 'Text.Read.Read'
-- instance. Take care not to use this to return Strings unless you really
-- want that, because it will require the quotation marks and escaping as
-- is normal for the Show/Read instances for String.
addParamRead
  :: forall out a
   . (Typeable a, Show a, Text.Read.Read a)
  => String -- ^ paramater name, for use in usage/help texts
  -> Param a -- ^ properties
  -> CmdParser out a
addParamRead name par = addCmdPart desc parseF
 where
  desc :: PartDesc
  desc =
    addSuggestion (_param_suggestions par)
      $ (maybe id PartWithHelp $ _param_help par)
      $ (maybe id (PartDefault . show) $ _param_default par)
      $ PartVariable name
  parseF :: String -> EpsilonFlag -> Maybe (a, String)
  parseF s e = case (Text.Read.reads s, e) of
    (((x, ' ' : r) : _), _           ) -> Just (x, dropWhile Char.isSpace r)
    (((x, []     ) : _), _           ) -> Just (x, [])
    (_                 , AllowEpsilon) -> _param_default par <&> \x -> (x, s)
    (_                 , DenyEpsilon ) -> Nothing

addSuggestion :: Maybe [CompletionItem] -> PartDesc -> PartDesc
addSuggestion Nothing     = id
addSuggestion (Just sugs) = PartSuggestion sugs

-- | Like addReadParam, but optional. I.e. if reading fails, returns Nothing.
addParamReadOpt
  :: forall out a
   . (Typeable a, Text.Read.Read a)
  => String -- ^ paramater name, for use in usage/help texts
  -> Param a -- ^ properties
  -> CmdParser out (Maybe a)
addParamReadOpt name par = addCmdPart desc parseF
 where
  desc :: PartDesc
  desc =
    addSuggestion (_param_suggestions par)
      $ PartOptional
      $ (maybe id PartWithHelp $ _param_help par)
      $ PartVariable name
  parseF :: String -> EpsilonFlag -> Maybe (Maybe a, String)
  parseF s e = case Text.Read.reads s of
    ((x, ' ' : r) : _) -> Just (Just x, dropWhile Char.isSpace r)
    ((x, []     ) : _) -> Just (Just x, [])
    _                  -> [ (Nothing, s) | e == AllowEpsilon ]


-- | Add a parameter that matches any string of non-space characters if
-- input==String, or one full argument if input==[String]. See the 'Input' doc
-- for this distinction.
addParamString :: String -> Param String -> CmdParser out String
addParamString name par = addCmdPartInp desc parseF
 where
  desc :: PartDesc
  desc =
    addSuggestion (_param_suggestions par)
      $ (maybe id PartWithHelp $ _param_help par)
      $ PartVariable name
  parseF :: Input -> EpsilonFlag -> Maybe (String, Input)
  parseF (InputString str) e =
    case break Char.isSpace $ dropWhile Char.isSpace str of
      ("", rest) ->
        [ (x, InputString rest) | x <- _param_default par, e == AllowEpsilon ]
      (x, rest) -> Just (x, InputString rest)
  parseF (InputArgs args) e = case args of
    (s1 : sR) -> Just (s1, InputArgs sR)
    [] -> [ (x, InputArgs args) | x <- _param_default par, e == AllowEpsilon ]

-- | Like 'addParamString', but optional, I.e. succeeding with Nothing if
-- there is no remaining input.
addParamStringOpt :: String -> Param Void -> CmdParser out (Maybe String)
addParamStringOpt name par = addCmdPartInp desc parseF
 where
  desc :: PartDesc
  desc =
    addSuggestion (_param_suggestions par)
      $ PartOptional
      $ (maybe id PartWithHelp $ _param_help par)
      $ PartVariable name
  parseF :: Input -> EpsilonFlag -> Maybe (Maybe String, Input)
  parseF (InputString str) e =
    case break Char.isSpace $ dropWhile Char.isSpace str of
      ("", rest) -> [ (Nothing, InputString rest) | e == AllowEpsilon ]
      (x , rest) -> Just (Just x, InputString rest)
  parseF (InputArgs args) e = case args of
    (s1 : sR) -> Just (Just s1, InputArgs sR)
    []        -> [ (Nothing, InputArgs []) | e == AllowEpsilon ]


-- | Add a parameter that matches any string of non-space characters if
-- input==String, or one full argument if input==[String]. See the 'Input' doc
-- for this distinction.
addParamStrings :: String -> Param Void -> CmdParser out [String]
addParamStrings name par = addCmdPartManyInp ManyUpperBoundN desc parseF
 where
  desc :: PartDesc
  desc =
    addSuggestion (_param_suggestions par)
      $ (maybe id PartWithHelp $ _param_help par)
      $ PartVariable name
  parseF :: Input -> EpsilonFlag -> Maybe (String, Input)
  parseF (InputString str) _e =
    case break Char.isSpace $ dropWhile Char.isSpace str of
      ("", _   ) -> Nothing
      (x , rest) -> Just (x, InputString rest)
  parseF (InputArgs args) _e = case args of
    (s1 : sR) -> Just (s1, InputArgs sR)
    []        -> Nothing


-- | Like 'addParamString' but does not match strings starting with a dash.
-- This prevents misinterpretation of flags as params.
addParamNoFlagString :: String -> Param String -> CmdParser out String
addParamNoFlagString name par = addCmdPartInp desc parseF
 where
  desc :: PartDesc
  desc =
    addSuggestion (_param_suggestions par)
      $ (maybe id PartWithHelp $ _param_help par)
      $ PartVariable name
  parseF :: Input -> EpsilonFlag -> Maybe (String, Input)
  parseF (InputString str) e =
    case break Char.isSpace $ dropWhile Char.isSpace str of
      ("", rest) ->
        [ (x, InputString rest) | x <- _param_default par, e == AllowEpsilon ]
      ('-' : _, _) ->
        [ (x, InputString str) | x <- _param_default par, e == AllowEpsilon ]
      (x, rest) -> Just (x, InputString rest)
  parseF (InputArgs args) e = case args of
    [] -> [ (x, InputArgs args) | x <- _param_default par, e == AllowEpsilon ]
    (('-' : _) : _) ->
      [ (x, InputArgs args) | x <- _param_default par, e == AllowEpsilon ]
    (s1 : sR) -> Just (s1, InputArgs sR)

-- | Like 'addParamStringOpt' but does not match strings starting with a dash.
-- This prevents misinterpretation of flags as params.
addParamNoFlagStringOpt :: String -> Param Void -> CmdParser out (Maybe String)
addParamNoFlagStringOpt name par = addCmdPartInp desc parseF
 where
  desc :: PartDesc
  desc =
    PartOptional $ (maybe id PartWithHelp $ _param_help par) $ PartVariable name
  parseF :: Input -> EpsilonFlag -> Maybe (Maybe String, Input)
  parseF (InputString str) e =
    case break Char.isSpace $ dropWhile Char.isSpace str of
      (""     , rest) -> [ (Nothing, InputString rest) | e == AllowEpsilon ]
      ('-' : _, _   ) -> [ (Nothing, InputString str) | e == AllowEpsilon ]
      (x      , rest) -> Just (Just x, InputString rest)
  parseF (InputArgs args) e = case args of
    []               -> [ (Nothing, InputArgs []) | e == AllowEpsilon ]
    (('-' : _) : _ ) -> [ (Nothing, InputArgs args) | e == AllowEpsilon ]
    (s1        : sR) -> Just (Just s1, InputArgs sR)

-- | Like 'addParamStrings' but does not match strings starting with a dash.
-- This prevents misinterpretation of flags as params.
addParamNoFlagStrings :: String -> Param Void -> CmdParser out [String]
addParamNoFlagStrings name par = addCmdPartManyInp ManyUpperBoundN desc parseF
 where
  desc :: PartDesc
  desc =
    addSuggestion (_param_suggestions par)
      $ (maybe id PartWithHelp $ _param_help par)
      $ PartVariable name
  parseF :: Input -> EpsilonFlag -> Maybe (String, Input)
  parseF (InputString str) _e =
    case break Char.isSpace $ dropWhile Char.isSpace str of
      (""     , _   ) -> Nothing
      ('-' : _, _   ) -> Nothing
      (x      , rest) -> Just (x, InputString rest)
  parseF (InputArgs args) _e = case args of
    []               -> Nothing
    (('-' : _) : _ ) -> Nothing
    (s1        : sR) -> Just (s1, InputArgs sR)


-- | Add a parameter that consumes _all_ remaining input. Typical usecase is
-- after a "--" as common in certain (unix?) commandline tools.
addParamRestOfInput :: String -> Param Void -> CmdParser out String
addParamRestOfInput name par = addCmdPartInp desc parseF
 where
  desc :: PartDesc
  desc =
    addSuggestion (_param_suggestions par)
      $ (maybe id PartWithHelp $ _param_help par)
      $ PartVariable name
  parseF :: Input -> EpsilonFlag -> Maybe (String, Input)
  parseF (InputString str ) _e = Just (str, InputString "")
  parseF (InputArgs   args) _e = Just (List.unwords args, InputArgs [])


-- | Add a parameter that consumes _all_ remaining input, returning a raw
-- 'Input' value.
addParamRestOfInputRaw :: String -> Param Void -> CmdParser out Input
addParamRestOfInputRaw name par = addCmdPartInp desc parseF
 where
  desc :: PartDesc
  desc =
    addSuggestion (_param_suggestions par)
      $ (maybe id PartWithHelp $ _param_help par)
      $ PartVariable name
  parseF :: Input -> EpsilonFlag -> Maybe (Input, Input)
  parseF i@InputString{} _e = Just (i, InputString "")
  parseF i@InputArgs{}   _e = Just (i, InputArgs [])

