{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ViewPatterns #-}

-- | Flags are arguments to your current command that are prefixed with "-" or
-- "--", for example "-v" or "--verbose". These flags can have zero or one
-- argument. (Butcher internally has more general concept of "CmdPart" that
-- could handle any number of arguments, so take this as what this module aims
-- to provide, not what you could theoretically implement on top of butcher).

-- Note that the current implementation only accepts "--foo param" but not
-- "--foo=param". Someone really ought to implement support for the latter
-- at some point :)
module UI.Butcher.Monadic.Flag
  ( Flag(..)
  , flagHelp
  , flagHelpStr
  , flagDefault
  , flagHidden
  , addSimpleBoolFlag
  , addSimpleBoolFlagA
  , addSimpleCountFlag
  , addFlagReadParam
  , addFlagReadParams
  -- , addFlagReadParamA
  , addFlagStringParam
  , addFlagStringParams
  -- , addFlagStringParamA
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

import           Data.List.Extra                ( firstJust )



-- TODO: perhaps move this to Types module and refactor all code to use it
newtype InpParseString a = InpParseString (StateS.StateT String Maybe a)
  deriving (Functor, Applicative, Monad, State.Class.MonadState String, Alternative, MonadPlus)

runInpParseString :: String -> InpParseString a -> Maybe (a, String)
runInpParseString s (InpParseString m) = StateS.runStateT m s

pExpect :: String -> InpParseString ()
pExpect s = InpParseString $ do
  inp <- StateS.get
  case List.stripPrefix s inp of
    Nothing   -> mzero
    Just rest -> StateS.put rest

pExpectEof :: InpParseString ()
pExpectEof =
  InpParseString $ StateS.get >>= \inp -> if null inp then pure () else mzero

-- pDropSpace :: InpParseString ()
-- pDropSpace = InpParseString $ StateS.modify (dropWhile (==' '))

pOption :: InpParseString () -> InpParseString ()
pOption m = m <|> return ()



-- | flag-description monoid. You probably won't need to use the constructor;
-- mzero or any (<>) of flag(Help|Default) works well.
data Flag p = Flag
  { _flag_help       :: Maybe PP.Doc
  , _flag_default    :: Maybe p
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

instance Monoid (Flag p) where
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

-- | Create a 'Flag' marked as hidden. Similar to hidden commands, hidden
-- flags will not included in pretty-printing (help, usage etc.)
--
-- This feature is not well tested yet.
flagHidden :: Flag p
flagHidden = mempty { _flag_visibility = Hidden }

wrapHidden :: Flag p -> PartDesc -> PartDesc
wrapHidden f = case _flag_visibility f of
  Visible -> id
  Hidden  -> PartHidden

-- | A no-parameter flag where non-occurence means False, occurence means True.
addSimpleBoolFlag
  :: Applicative f
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, e.g. ["verbose"]
  -> Flag Void -- ^ properties
  -> CmdParser f out Bool
addSimpleBoolFlag shorts longs flag =
  addSimpleBoolFlagAll shorts longs flag (pure ())

-- | Applicative-enabled version of 'addSimpleBoolFlag'
addSimpleBoolFlagA
  :: String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, e.g. ["verbose"]
  -> Flag Void -- ^ properties
  -> f () -- ^ action to execute whenever this matches
  -> CmdParser f out ()
addSimpleBoolFlagA shorts longs flag act =
  void $ addSimpleBoolFlagAll shorts longs flag act

addSimpleBoolFlagAll
  :: String -> [String] -> Flag Void -> f () -> CmdParser f out Bool
addSimpleBoolFlagAll shorts longs flag a = fmap (not . null)
  $ addCmdPartManyA ManyUpperBound1 (wrapHidden flag desc) parseF (\() -> a)
 where
  allStrs = fmap (\c -> "-" ++ [c]) shorts ++ fmap (\s -> "--" ++ s) longs
  desc :: PartDesc
  desc =
    (maybe id PartWithHelp $ _flag_help flag)
      $   PartAlts
      $   PartLiteral
      <$> allStrs
  parseF :: PartParser () String
  parseF (dropWhile Char.isSpace -> str) =
    resultFromMaybe
      $   (firstJust (\s -> [ ((), drop (length s) str) | s == str ]) allStrs)
      <|> (firstJust
            (\s ->
              [ ((), drop (length s + 1) str) | (s ++ " ") `isPrefixOf` str ]
            )
            allStrs
          )

-- | A no-parameter flag that can occur multiple times. Returns the number of
-- occurences (0 or more).
addSimpleCountFlag
  :: Applicative f
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> Flag Void -- ^ properties
  -> CmdParser f out Int
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
  parseF :: PartParser () String
  parseF (dropWhile Char.isSpace -> str) =
    resultFromMaybe
      $   (firstJust (\s -> [ ((), drop (length s) str) | s == str ]) allStrs)
      <|> (firstJust
            (\s ->
              [ ((), drop (length s + 1) str) | (s ++ " ") `isPrefixOf` str ]
            )
            allStrs
          )


-- can have one of
--   1) no default 2) default is nothing + just value 3) default value
-- inner default only makes sense if there is an outer default

-- | One-argument flag, where the argument is parsed via its Read instance.
addFlagReadParam
  :: forall f p out
   . (Applicative f, Typeable p, Text.Read.Read p, Show p)
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> String -- ^ param name
  -> Flag p -- ^ properties
  -> CmdParser f out p
addFlagReadParam shorts longs name flag = addCmdPartInpA
  (wrapHidden flag desc)
  parseF
  (\_ -> pure ())
 where
  allStrs =
    [ Left $ "-" ++ [c] | c <- shorts ] ++ [ Right $ "--" ++ l | l <- longs ]
  desc =
    (maybe id PartWithHelp $ _flag_help flag)
      $ maybe id (PartDefault . show) (_flag_default flag)
      $ PartSeq [desc1, desc2]
  desc1 :: PartDesc
  desc1 = PartAlts $ PartLiteral . either id id <$> allStrs
  desc2 = PartVariable name
  parseF :: PartParser p Input
  parseF inp = case inp of
    InputString str -> case parseResult of
      Nothing -> resultFromMaybe $ _flag_default flag <&> \x -> (x, inp)
      Just (descOrVal, r) -> case descOrVal of
        Left  e   -> Failure (Just e)
        Right val -> Success val (InputString r)
     where
      parseResult = runInpParseString (dropWhile Char.isSpace str) $ do
        Data.Foldable.msum $ allStrs <&> \case
          Left  s -> pExpect s *> pOption (pExpect " " <|> pExpect "=")
          Right s -> pExpect s *> (pExpect " " <|> pExpect "=")
        InpParseString $ do
          i <- StateS.get
          case Text.Read.reads i of
            ((x, ' ' : r) : _) ->
              StateS.put (dropWhile Char.isSpace r) $> Right x
            ((x, "") : _) -> StateS.put "" $> Right x
            _             -> pure $ Left desc2
    InputArgs (arg1 : argR) -> case runInpParseString arg1 parser of
      Just ((), "") -> case argR of
        []            -> Failure Nothing
        (arg2 : rest) -> case Text.Read.readMaybe arg2 of
          Just x  -> Success x (InputArgs rest)
          Nothing -> Failure (Just desc2)
      Just ((), remainingStr) -> case Text.Read.readMaybe remainingStr of
        Just x  -> Success x (InputArgs argR)
        Nothing -> Failure (Just desc2)
      Nothing -> resultFromMaybe $ _flag_default flag <&> \d -> (d, inp)
     where
      parser :: InpParseString ()
      parser = do
        Data.Foldable.msum $ allStrs <&> \case
          Left  s -> pExpect s *> pOption (pExpect "=")
          Right s -> pExpect s *> (pExpect "=" <|> pExpectEof)
    InputArgs _ -> resultFromMaybe $ _flag_default flag <&> \d -> (d, inp)

-- | One-argument flag, where the argument is parsed via its Read instance.
-- This version can accumulate multiple values by using the same flag with
-- different arguments multiple times.
--
-- E.g. "--foo 3 --foo 5" yields [3,5].
addFlagReadParams
  :: forall f p out
   . (Applicative f, Typeable p, Text.Read.Read p, Show p)
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> String -- ^ param name
  -> Flag p -- ^ properties
  -> CmdParser f out [p]
addFlagReadParams shorts longs name flag =
  addFlagReadParamsAll shorts longs name flag (\_ -> pure ())

-- TODO: this implementation is wrong, because it uses addCmdPartManyInpA
--       while this really is no Many.
-- | Applicative-enabled version of 'addFlagReadParam'
-- addFlagReadParamA
--   :: forall f p out
--    . (Typeable p, Text.Read.Read p, Show p)
--   => String -- ^ short flag chars, i.e. "v" for -v
--   -> [String] -- ^ list of long names, i.e. ["verbose"]
--   -> String -- ^ param name
--   -> Flag p -- ^ properties
--   -> (p -> f ()) -- ^ action to execute when ths param matches
--   -> CmdParser f out ()
-- addFlagReadParamA shorts longs name flag act
--   = void $ addFlagReadParamsAll shorts longs name flag act

addFlagReadParamsAll
  :: forall f p out
   . (Typeable p, Text.Read.Read p, Show p)
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> String -- ^ param name
  -> Flag p -- ^ properties
  -> (p -> f ()) -- ^ action to execute when ths param matches
  -> CmdParser f out [p]
addFlagReadParamsAll shorts longs name flag act = addCmdPartManyInpA
  ManyUpperBoundN
  (wrapHidden flag desc)
  parseF
  act
 where
  allStrs =
    [ Left $ "-" ++ [c] | c <- shorts ] ++ [ Right $ "--" ++ l | l <- longs ]
  desc = (maybe id PartWithHelp $ _flag_help flag) $ PartSeq [desc1, desc2]
  desc1 :: PartDesc
  desc1 = PartAlts $ PartLiteral . either id id <$> allStrs
  desc2 =
    (maybe id (PartDefault . show) $ _flag_default flag) $ PartVariable name
  parseF :: PartParser p Input
  parseF inp = case inp of
    InputString str -> case parseResult of
      Just (descOrVal, r) -> case descOrVal of
        Right val -> Success val (InputString r)
        Left  err -> Failure (Just err)
      Nothing -> Failure Nothing
     where
      parseResult = runInpParseString (dropWhile Char.isSpace str) $ do
        Data.Foldable.msum $ allStrs <&> \case
          Left  s -> pExpect s *> pOption (pExpect " " <|> pExpect "=")
          Right s -> pExpect s *> (pExpect " " <|> pExpect "=")
        InpParseString $ do
          i <- StateS.get
          case Text.Read.reads i of
            ((x, ' ' : r) : _) ->
              StateS.put (dropWhile Char.isSpace r) $> Right x
            ((x, "") : _) -> StateS.put "" $> Right x
            _             -> pure $ case _flag_default flag of
              Nothing  -> Left desc2
              Just val -> Right val
    InputArgs (arg1 : argR) -> case runInpParseString arg1 parser of
      Just ((), "") -> case argR of
        []            -> mdef
        (arg2 : rest) -> case Text.Read.readMaybe arg2 of
          Just x  -> Success x (InputArgs rest)
          Nothing -> mdef
       where
        mdef = case _flag_default flag of
          Nothing  -> Failure (Just desc2)
          Just val -> Success val (InputArgs argR)
      Just ((), remainingStr) -> case Text.Read.readMaybe remainingStr of
        Just x  -> Success x (InputArgs argR)
        Nothing -> Failure (Just desc2) -- this is a bit questionable,
                  -- could also make it Nothing.
      Nothing -> Failure Nothing
     where
      parser :: InpParseString ()
      parser = do
        Data.Foldable.msum $ allStrs <&> \case
          Left  s -> pExpect s *> pOption (pExpect "=")
          Right s -> pExpect s *> (pExpect "=" <|> pExpectEof)
    InputArgs _ -> Failure Nothing

-- | One-argument flag where the argument can be an arbitrary string.
addFlagStringParam
  :: forall f out
   . (Applicative f)
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> String -- ^ param name
  -> Flag String -- ^ properties
  -> CmdParser f out String
addFlagStringParam shorts longs name flag = addCmdPartInpA
  (wrapHidden flag desc)
  parseF
  (\_ -> pure ())
 where
  allStrs =
    [ Left $ "-" ++ [c] | c <- shorts ] ++ [ Right $ "--" ++ l | l <- longs ]
  desc =
    (maybe id PartWithHelp $ _flag_help flag)
      $ maybe id (PartDefault . show) (_flag_default flag)
      $ PartSeq [desc1, desc2]
  desc1 :: PartDesc
  desc1 = PartAlts $ PartLiteral . either id id <$> allStrs
  desc2 = PartVariable name
  parseF :: PartParser String Input
  parseF inp = case inp of
    InputString str -> case parseResult of
      Nothing -> resultFromMaybe $ _flag_default flag <&> \x -> (x, inp)
      Just (descOrVal, r) -> case descOrVal of
        Left  e   -> Failure (Just e)
        Right val -> Success val (InputString r)
     where
      parseResult = runInpParseString (dropWhile Char.isSpace str) $ do
        Data.Foldable.msum $ allStrs <&> \case
          Left  s -> pExpect s *> pOption (pExpect " " <|> pExpect "=")
          Right s -> pExpect s *> (pExpect " " <|> pExpect "=")
        InpParseString $ do
          i <- StateS.get
          let (x, rest) = break Char.isSpace $ dropWhile Char.isSpace i
          StateS.put rest
          pure $ Right x
    InputArgs (arg1 : argR) -> case runInpParseString arg1 parser of
      Just ((), "") -> case argR of
        []         -> Failure Nothing
        (x : rest) -> Success x (InputArgs rest)
      Just ((), remainingStr) -> case Text.Read.readMaybe remainingStr of
        Just x  -> Success x (InputArgs argR)
        Nothing -> Failure (Just desc2)
      Nothing -> resultFromMaybe $ _flag_default flag <&> \d -> (d, inp)
     where
      parser :: InpParseString ()
      parser = do
        Data.Foldable.msum $ allStrs <&> \case
          Left  s -> pExpect s *> pOption (pExpect "=")
          Right s -> pExpect s *> (pExpect "=" <|> pExpectEof)
    InputArgs _ -> resultFromMaybe $ _flag_default flag <&> \d -> (d, inp)

-- | One-argument flag where the argument can be an arbitrary string.
-- This version can accumulate multiple values by using the same flag with
-- different arguments multiple times.
--
-- E.g. "--foo abc --foo def" yields ["abc", "def"].
addFlagStringParams
  :: forall f out
   . (Applicative f)
  => String -- ^ short flag chars, i.e. "v" for -v
  -> [String] -- ^ list of long names, i.e. ["verbose"]
  -> String -- ^ param name
  -> Flag Void -- ^ properties
  -> CmdParser f out [String]
addFlagStringParams shorts longs name flag =
  addFlagStringParamsAll shorts longs name flag (\_ -> pure ())

-- TODO: this implementation is wrong, because it uses addCmdPartManyInpA
--       while this really is no Many.
-- -- | Applicative-enabled version of 'addFlagStringParam'
-- addFlagStringParamA
--   :: forall f out
--   .  String -- ^ short flag chars, i.e. "v" for -v
--   -> [String] -- ^ list of long names, i.e. ["verbose"]
--   -> String -- ^ param name
--   -> Flag Void -- ^ properties
--   -> (String -> f ()) -- ^ action to execute when ths param matches
--   -> CmdParser f out ()
-- addFlagStringParamA shorts longs name flag act
--   = void $ addFlagStringParamsAll shorts longs name flag act

addFlagStringParamsAll
  :: forall f out
   . String
  -> [String]
  -> String
  -> Flag Void -- we forbid the default because it has bad interaction
               -- with the eat-anything behaviour of the string parser.
  -> (String -> f ())
  -> CmdParser f out [String]
addFlagStringParamsAll shorts longs name flag act = addCmdPartManyInpA
  ManyUpperBoundN
  (wrapHidden flag desc)
  parseF
  act
 where
  allStrs =
    [ Left $ "-" ++ [c] | c <- shorts ] ++ [ Right $ "--" ++ l | l <- longs ]
  desc = (maybe id PartWithHelp $ _flag_help flag) $ PartSeq [desc1, desc2]
  desc1 :: PartDesc
  desc1 = PartAlts $ PartLiteral . either id id <$> allStrs
  desc2 =
    (maybe id (PartDefault . show) $ _flag_default flag) $ PartVariable name
  parseF :: PartParser String Input
  parseF inp = case inp of
    InputString str ->
      resultFromMaybe $ fmap (second InputString) $ parseResult
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
        []         -> Failure Nothing
        (x : rest) -> Success x (InputArgs rest)
      Just ((), remainingStr) -> Success remainingStr (InputArgs argR)
      Nothing                 -> Failure Nothing
     where
      parser :: InpParseString ()
      parser = do
        Data.Foldable.msum $ allStrs <&> \case
          Left  s -> pExpect s *> pOption (pExpect "=")
          Right s -> pExpect s *> (pExpect "=" <|> pExpectEof)
    InputArgs _ -> Failure Nothing
