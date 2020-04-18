{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}


module UI.Butcher.Applicative.Flag
  ( Flag(..)
  , flagDefault
  , flagHelp
  , flagHelpStr
  , addSimpleBoolFlag
  , addSimpleCountFlag
  , addFlagReadParam
  , addFlagReadParams
  , addFlagStringParam
  )
where



#include "prelude.inc"

import           Control.Applicative.Free
import           Control.Monad.ST
import           Data.Kind
import           Data.List.Extra                ( firstJust )
import           Data.STRef
import qualified Text.PrettyPrint              as PP

import           UI.Butcher.Applicative.Param
import           UI.Butcher.Internal.ApplicativeTypes
import           UI.Butcher.Internal.Applicative
import           UI.Butcher.Internal.BasicStringParser
import           UI.Butcher.Internal.Pretty

import           Debug.Trace



data Flag a = Flag
  { _flag_help       :: Maybe PP.Doc
  , _flag_default    :: Maybe a
  , _flag_visibility :: Visibility
  }


appendFlag :: Flag p -> Flag p -> Flag p
appendFlag (Flag a1 b1 c1) (Flag a2 b2 c2) = Flag (a1 <|> a2)
                                                  (b1 <|> b2)
                                                  (appVis c1 c2)
 where
  appVis Visible Visible = Visible
  appVis _       _       = Hidden

instance Semigroup (Flag p) where
  (<>) = appendFlag

instance Monoid (Flag a) where
  mempty  = Flag Nothing Nothing Visible
  mappend = (<>)

-- | Create a 'Flag' with just a help text.
flagHelp :: PP.Doc -> Flag p
flagHelp h = mempty { _flag_help = Just h }

-- | Create a 'Flag' with just a help text.
flagHelpStr :: String -> Flag p
flagHelpStr s =
  mempty { _flag_help = Just $ PP.fsep $ fmap PP.text $ List.words s }

-- | Create a 'Flag' with just a default value.
flagDefault :: p -> Flag p
flagDefault d = mempty { _flag_default = Just d }

wrapHidden :: Flag p -> PartDesc -> PartDesc
wrapHidden f = case _flag_visibility f of
  Visible -> id
  Hidden  -> PartHidden


addSimpleBoolFlag :: String -> [String] -> Flag Void -> CmdParser out Bool
addSimpleBoolFlag shorts longs opts = fmap (not . null)
  $ addCmdPartMany ManyUpperBound1 (wrapHidden opts desc) parseF
 where
  allStrs = fmap (\c -> "-" ++ [c]) shorts ++ fmap (\s -> "--" ++ s) longs
  desc :: PartDesc
  desc =
    (maybe id PartWithHelp $ _flag_help opts)
      $   PartAlts
      $   PartLiteral
      <$> allStrs
  parseF :: String -> EpsilonFlag -> Maybe ((), String)
  parseF (dropWhile Char.isSpace -> str) _ =
    (firstJust (\s -> [ ((), drop (length s) str) | s == str ]) allStrs)
      <|> (firstJust
            (\s ->
              [ ((), drop (length s + 1) str) | (s ++ " ") `isPrefixOf` str ]
            )
            allStrs
          )


-- | A no-parameter flag that can occur multiple times. Returns the number of
-- occurences (0 or more).
addSimpleCountFlag
  :: String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> Flag Void -- ^ properties
  -> CmdParser out Int
addSimpleCountFlag shorts longs flag = fmap length
  $ addCmdPartMany ManyUpperBoundN (wrapHidden flag desc) parseF
 where
    -- we _could_ allow this to parse repeated short flags, like "-vvv"
    -- (meaning "-v -v -v") correctly.
  allStrs = fmap (\c -> "-" ++ [c]) shorts ++ fmap (\s -> "--" ++ s) longs
  desc :: PartDesc
  desc =
    (maybe id PartWithHelp $ _flag_help flag)
      $   PartAlts
      $   PartLiteral
      <$> allStrs
  parseF :: String -> EpsilonFlag -> Maybe ((), String)
  parseF (dropWhile Char.isSpace -> str) _ =
    (firstJust (\s -> [ ((), drop (length s) str) | s == str ]) allStrs)
      <|> (firstJust
            (\s ->
              [ ((), drop (length s + 1) str) | (s ++ " ") `isPrefixOf` str ]
            )
            allStrs
          )


addFlagReadParam
  :: forall out p
   . (Typeable p, Read p, Show p)
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> String -- ^ param name
  -> Flag p -- ^ properties
  -> CmdParser out p
addFlagReadParam shorts longs name opts = addCmdPartInp
  (wrapHidden opts desc)
  parseF
 where
  allStrs =
    [ Left $ "-" ++ [c] | c <- shorts ] ++ [ Right $ "--" ++ l | l <- longs ]
  desc =
    (maybe id PartWithHelp $ _flag_help opts)
      $ maybe id (PartDefault . show) (_flag_default opts)
      $ PartSeq [desc1, desc2]
  desc1 :: PartDesc
  desc1 = PartAlts $ PartLiteral . either id id <$> allStrs
  desc2 = PartVariable name
  parseF :: Input -> EpsilonFlag -> Maybe (p, Input)
  parseF inp e = case inp of
    InputString str -> case parseResult of
      Just (x, rest) -> Just (x, InputString rest)
      Nothing        -> viaDef
     where
      parseResult = runInpParseString (dropWhile Char.isSpace str) $ do
        Data.Foldable.msum $ allStrs <&> \case
          Left  s -> pExpect s *> pOption (pExpect " " <|> pExpect "=")
          Right s -> pExpect s *> (pExpect " " <|> pExpect "=")
        InpParseString $ do
          i <- StateS.get
          case Text.Read.reads i of
            ((x, ' ' : r) : _) -> StateS.put (dropWhile Char.isSpace r) $> x
            ((x, ""     ) : _) -> StateS.put "" $> x
            _                  -> mzero
    InputArgs (arg1 : argR) -> case runInpParseString arg1 parser of
      Just ((), "") -> case argR of
        []            -> Nothing
        (arg2 : rest) -> Text.Read.readMaybe arg2 <&> \x -> (x, InputArgs rest)
      Just ((), remainingStr) ->
        Text.Read.readMaybe remainingStr <&> \x -> (x, InputArgs argR)
      Nothing -> viaDef
     where
      parser :: InpParseString ()
      parser = do
        Data.Foldable.msum $ allStrs <&> \case
          Left  s -> pExpect s *> pOption (pExpect "=")
          Right s -> pExpect s *> (pExpect "=" <|> pExpectEof)
    InputArgs _ -> viaDef
    where viaDef = [ (x, inp) | x <- _flag_default opts, e == AllowEpsilon ]


-- | One-argument flag, where the argument is parsed via its Read instance.
-- This version can accumulate multiple values by using the same flag with
-- different arguments multiple times.
--
-- E.g. "--foo 3 --foo 5" yields [3,5].
addFlagReadParams
  :: forall p out
   . (Typeable p, Read p, Show p)
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> String -- ^ param name
  -> Flag p -- ^ properties
  -> CmdParser out [p]
addFlagReadParams shorts longs name flag = addCmdPartManyInp
  ManyUpperBoundN
  (wrapHidden flag desc)
  parseF
 where
  allStrs =
    [ Left $ "-" ++ [c] | c <- shorts ] ++ [ Right $ "--" ++ l | l <- longs ]
  desc = (maybe id PartWithHelp $ _flag_help flag) $ PartSeq [desc1, desc2]
  desc1 :: PartDesc
  desc1 = PartAlts $ PartLiteral . either id id <$> allStrs
  desc2 =
    (maybe id (PartDefault . show) $ _flag_default flag) $ PartVariable name
  parseF :: Input -> EpsilonFlag -> Maybe (p, Input)
  parseF inp _ = case inp of
    InputString str -> fmap (second InputString) $ parseResult
     where
      parseResult = runInpParseString (dropWhile Char.isSpace str) $ do
        Data.Foldable.msum $ allStrs <&> \case
          Left  s -> pExpect s *> pOption (pExpect " " <|> pExpect "=")
          Right s -> pExpect s *> (pExpect " " <|> pExpect "=")
        InpParseString $ do
          i <- StateS.get
          case Text.Read.reads i of
            ((x, ' ' : r) : _) -> StateS.put (dropWhile Char.isSpace r) $> x
            ((x, ""     ) : _) -> StateS.put "" $> x
            _                  -> lift $ _flag_default flag
    InputArgs (arg1 : argR) -> case runInpParseString arg1 parser of
      Just ((), "") -> case argR of
        [] -> mdef
        (arg2 : rest) ->
          (Text.Read.readMaybe arg2 <&> \x -> (x, InputArgs rest)) <|> mdef
        where mdef = _flag_default flag <&> \p -> (p, InputArgs argR)
      Just ((), remainingStr) ->
        Text.Read.readMaybe remainingStr <&> \x -> (x, InputArgs argR)
      Nothing -> Nothing
     where
      parser :: InpParseString ()
      parser = do
        Data.Foldable.msum $ allStrs <&> \case
          Left  s -> pExpect s *> pOption (pExpect "=")
          Right s -> pExpect s *> (pExpect "=" <|> pExpectEof)
    InputArgs _ -> Nothing


-- | One-argument flag where the argument can be an arbitrary string.
addFlagStringParam
  :: forall out
   . String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> String -- ^ param name
  -> Flag String -- ^ properties
  -> CmdParser out String
addFlagStringParam shorts longs name opts = addCmdPartInp
  (wrapHidden opts desc)
  parseF
 where
  allStrs =
    [ Left $ "-" ++ [c] | c <- shorts ] ++ [ Right $ "--" ++ l | l <- longs ]
  desc = (maybe id PartWithHelp $ _flag_help opts) $ PartSeq [desc1, desc2]
  desc1 :: PartDesc
  desc1 = PartAlts $ PartLiteral . either id id <$> allStrs
  desc2 = PartVariable name
  parseF :: Input -> EpsilonFlag -> Maybe (String, Input)
  parseF inp e = case inp of
    InputString str -> case parseResult of
      Just (x, rest) -> Just (x, InputString rest)
      Nothing        -> viaDef
     where
      parseResult = runInpParseString (dropWhile Char.isSpace str) $ do
        Data.Foldable.msum $ allStrs <&> \case
          Left  s -> pExpect s *> pOption (pExpect " " <|> pExpect "=")
          Right s -> pExpect s *> (pExpect " " <|> pExpect "=")
        InpParseString $ do
          i <- StateS.get
          let (x, rest) = break Char.isSpace $ dropWhile Char.isSpace i
          StateS.put rest
          pure x
    InputArgs (arg1 : argR) -> case runInpParseString arg1 parser of
      Just ((), "") -> case argR of
        []         -> Nothing
        (x : rest) -> Just (x, InputArgs rest)
      Just ((), remainingStr) -> Just (remainingStr, InputArgs argR)
      Nothing                 -> viaDef
     where
      parser :: InpParseString ()
      parser = do
        Data.Foldable.msum $ allStrs <&> \case
          Left  s -> pExpect s *> pOption (pExpect "=")
          Right s -> pExpect s *> (pExpect "=" <|> pExpectEof)
    InputArgs _ -> viaDef
    where viaDef = [ (x, inp) | x <- _flag_default opts, e == AllowEpsilon ]
