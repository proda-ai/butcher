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

module UI.Butcher.Internal.Applicative
  ( -- runCmdParser
  -- , runCmdParserWithHelpDesc
  -- , runCmdParserSimple
    runCmdParserCoreFromDesc
  , toCmdDesc
  , traverseBarbie
  , addCmd
  , addCmdHidden
  , addCmdPart
  , addCmdPartMany
  , addCmdPartInp
  , addCmdPartManyInp
  , peekCmdDesc
  , reorderStart
  , reorderStop
  )
where




#include "prelude.inc"

import qualified Barbies
import qualified Barbies.Bare                  as Barbies
import           Control.Applicative.Free
import           Control.Monad.ST
import           Data.STRef
import qualified Text.PrettyPrint              as PP

import           UI.Butcher.Internal.ApplicativeTypes
import           UI.Butcher.Internal.CommonTypes
import           UI.Butcher.Internal.Interactive
import           UI.Butcher.Internal.Pretty



data DescState = DescState
  { parts    :: Deque PartDesc
  , children :: Deque (String, CommandDesc)
  , help     :: Maybe PP.Doc
  , reorder  :: Maybe (Deque PartDesc)
  }

toCmdDesc :: forall out . CmdParser out out -> CommandDesc
toCmdDesc cmdParser =
  let final = appEndo (runAp_ f cmdParser) initialState
  in  CommandDesc { _cmd_mParent    = Nothing
                  , _cmd_synopsis   = Nothing
                  , _cmd_help       = help final
                  , _cmd_parts      = Data.Foldable.toList $ parts final
                  , _cmd_hasImpl    = True -- all applicatives have an impl atm
                  , _cmd_children   = fmap (first Just) $ children final
                  , _cmd_visibility = Visible
                  }
 where
  f :: CmdParserF out a -> Endo (DescState)
  f x = Endo $ \s -> case x of
    CmdParserHelp     doc _         -> s { help = Just doc }
    CmdParserSynopsis _   _         -> error "todo"
    CmdParserPeekDesc _             -> s
    CmdParserPeekInput _            -> s
    -- CmdParserPart desc _ _          -> appendPart s desc
    -- CmdParserPartMany _ desc _ _    -> appendPart s desc
    CmdParserPartInp desc _ _       -> appendPart s desc
    CmdParserPartManyInp _ desc _ _ -> appendPart s desc
    CmdParserChild name vis parser _ ->
      appendChild s $ (name, (toCmdDesc parser) { _cmd_visibility = vis })
    CmdParserReorderStart _ -> s { reorder = reorder s <|> Just empty }
    CmdParserReorderStop  _ -> case reorder s of
      Nothing -> s
      Just ps -> s { parts = parts s <> ps, reorder = Nothing }
   where
    appendPart s p = s { parts = Deque.cons p (parts s) }
    appendChild s c = s { children = Deque.cons c (children s) }
  initialState = DescState { parts    = mempty
                           , children = mempty
                           , help     = Nothing
                           , reorder  = mempty
                           }

data ParserState out = ParserState
  { p_parts    :: Deque PartDesc
  , p_children :: Deque (String, CommandDesc)
  , p_help     :: Maybe PP.Doc
  , p_reorder  :: Maybe (Deque PartDesc)
  , p_input    :: Input
  , p_currentDesc :: CommandDesc
  }

runCmdParserCoreFromDesc
  :: forall out
   . Input
  -> CommandDesc
  -> CmdParser out out
  -> (CommandDesc, Input, Either ParsingError out)
runCmdParserCoreFromDesc input desc parser =
  let initialState = ParserState { p_parts    = mempty
                                 , p_children = mempty
                                 , p_help     = Nothing
                                 , p_reorder  = mempty
                                 , p_input    = input
                                 , p_currentDesc = desc
                                 }
      (result, finalState) = runST $ StateS.runStateT (iter parser) initialState
  in  (desc, p_input finalState, result)
 where
  iter
    :: forall s
     . CmdParser out out
    -> StateS.StateT (ParserState out) (ST s) (Either ParsingError out)
  iter = \case
    Pure x                          -> pure $ Right x
    Ap (CmdParserHelp     _ x) next -> continue next x
    Ap (CmdParserSynopsis _ x) next -> continue next x
    Ap (CmdParserPeekDesc f) next -> do
      s <- StateS.get
      iter $ next <*> Pure (f (p_currentDesc s))
    Ap (CmdParserPeekInput f ) next -> do
      s <- StateS.get
      iter $ next <*> Pure (f (inputToString $ p_input s))
    Ap (CmdParserPartInp _d parseF f) next -> do
      s <- StateS.get
      case parseF (p_input s) AllowEpsilon of
        Just (x, rest) -> do
          StateS.put s { p_input = rest }
          iter $ next <&> \g -> g (f x)
        Nothing -> pure $ Left $ ParsingError
          { _pe_messages     = ["could not parse"]
          , _pe_remaining    = p_input s
          , _pe_expectedDesc = Nothing -- TODO
          }
    Ap (CmdParserPartManyInp _ _ parseF f) next -> do
      let loop = do
            dropSpaces
            s <- StateS.get
            case parseF (p_input s) AllowEpsilon of
              Just (x, rest) -> do
                StateS.put s { p_input = rest }
                (x :) <$> loop
              Nothing -> pure $ []
      ps <- loop
      iter $ next <&> \g -> g (f ps)
    Ap (CmdParserChild name _ childParser x) next -> do
      dropSpaces
      s <- StateS.get
      let childDesc = case find ((== Just name) . fst) (_cmd_children desc) of
            Just (_, d) -> d
            Nothing     -> error "inconsistent child name map"
      case p_input s of
        InputString str -> if
          | str == name -> do
            StateS.put ParserState { p_parts    = mempty
                                 , p_children = mempty
                                 , p_help     = Nothing
                                 , p_reorder  = mempty
                                 , p_input    = InputString ""
                                 , p_currentDesc = childDesc
                                 }
            iter childParser
          |
-- TODO str prefix
            otherwise -> continue next x
        InputArgs (a1 : ar) | a1 == name -> do
          StateS.put ParserState { p_parts    = mempty
                                 , p_children = mempty
                                 , p_help     = Nothing
                                 , p_reorder  = mempty
                                 , p_input    = InputArgs ar
                                 , p_currentDesc = childDesc
                                 }
          iter childParser
        InputArgs{} -> continue next x
    Ap (CmdParserReorderStart startX) next -> Except.runExceptT $ do
      let
        enrich
          :: forall a
           . CmdParser out a
          -> StateS.StateT
               (ParserState out)
               (ST s)
               (Ap (EnrichedCmdParserF s out) a, [ReorderUnit s])
        enrich = \case
          Ap (CmdParserPartInp _ parseF f) n -> do
            ref         <- lift $ newSTRef Nothing
            (n', units) <- enrich n
            pure (Ap (ViaRef ref f) n', ReorderUnit ref parseF : units)
          Ap (CmdParserPartManyInp bound _ parseF f) n -> do
            ref         <- lift $ newSTRef []
            (n', units) <- enrich n
            pure
              ( Ap (ViaRefMany ref f) n'
              , ReorderUnitMany bound ref parseF : units
              )
          Ap (CmdParserReorderStop x) n -> do
            pure $ (liftAp $ Final (n <*> Pure x), [])
          Ap x n -> do
            (n', units) <- enrich n
            pure (Ap (Lifted x) n', units)
          Pure x -> do
            pure (Pure x, [])
        consumeReordered
          :: [ReorderUnit s]
          -> StateS.StateT (ParserState out) (ST s) [ReorderUnit s]
        consumeReordered units = do
          s <- StateS.get
          let
            matchF = \case
              ReorderUnit ref parseF ->
                case parseF (p_input s) DenyEpsilon of
                  Nothing        -> Nothing
                  Just (x, rest) -> Just $ \newUnits -> do
                    lift $ writeSTRef ref (Just x)
                    StateS.put s { p_input = rest }
                    consumeReordered newUnits
              ReorderUnitMany bound ref parseF ->
                case parseF (p_input s) DenyEpsilon of
                  Nothing        -> Nothing
                  Just (x, rest) -> Just $ \newUnits -> do
                    lift $ modifySTRef ref (x :)
                    StateS.put s { p_input = rest }
                    consumeReordered
                      $ if bound == ManyUpperBound1 then newUnits else units
          let (newUnits, mAct) = extract matchF units
          case mAct of
            Nothing  -> pure units
            Just act -> act newUnits
        derich
          :: forall a
           . Ap (EnrichedCmdParserF s out) a
          -> ST s (CmdParser out a)
        derich = \case
          Ap (ViaRef ref f) n -> do
            m <- readSTRef ref
            case m of
              Nothing -> error "butcher intenal error - reorder ref Nothing"
              Just x  -> derich $ n <*> Pure (f x)
          Ap (ViaRefMany ref f) n -> do
            x <- readSTRef ref
            derich $ n <*> Pure (f $ reverse x)
          Ap (Lifted l) n -> Ap l <$> derich n
          Ap (Final  f) n -> do
            n' <- derich n
            pure $ n' <*> f
          Pure x -> pure $ Pure x

      (e, units)     <- lift $ enrich (next <*> Pure startX)
      remainingUnits <- lift $ consumeReordered units
      remainingUnits `forM_` \case
        ReorderUnit ref parseF -> case parseF (InputArgs []) AllowEpsilon of
          Nothing -> do
            s <- State.Class.get
            Except.throwE ParsingError { _pe_messages     = ["could not parse"]
                                       , _pe_remaining    = p_input s
                                       , _pe_expectedDesc = Nothing -- TODO
                                       }
          Just (x, _) -> do
            lift $ lift $ writeSTRef ref (Just x)
        ReorderUnitMany{} -> pure ()
      Except.ExceptT $ iter =<< lift (derich e)
    Ap (CmdParserReorderStop _) next -> error "TODO" next
   where
    continue
      :: Ap (CmdParserF out) (a -> out)
      -> a
      -> StateS.StateT (ParserState out) (ST s1) (Either ParsingError out)
    continue next x = iter (next <*> Pure x)
    inputToString :: Input -> String
    inputToString (InputString s ) = s
    inputToString (InputArgs   ss) = List.unwords ss
    dropSpaces :: forall m . Monad m => StateS.StateT (ParserState out) m ()
    dropSpaces = do
      st <- StateS.get
      case p_input st of
        InputString s ->
          StateS.put $ st { p_input = InputString $ dropWhile Char.isSpace s }
        InputArgs{} -> return ()


-- | If you have a higher-kinded config type (let's assume it is a plain
-- record) then this turns a record whose fields are @CmdParser@s over
-- different values into a CmdParser that returns a record with the parsed
-- values in the fields.
--
-- See the BarbieParsing example included in this package.
traverseBarbie
  :: (Barbies.BareB c, Barbies.TraversableB (c Barbies.Covered))
  => c Barbies.Covered (CmdParser out)
  -> CmdParser out (c Barbies.Bare Identity)
traverseBarbie k = do
  r <- Barbies.btraverse (fmap Identity) k
  pure $ Barbies.bstrip r


-- | Add part that is expected to occur exactly once in the input.
-- The EpsilonFlag specifies whether succeeding on empty input is permitted
-- or not.
addCmdPart
  :: Typeable p
  => PartDesc
  -> (String -> EpsilonFlag -> Maybe (p, String))
  -> CmdParser out p
addCmdPart p f = liftAp $ CmdParserPartInp p (convertStringToInputParse f) id

-- | Add part that is not required to occur, and can occur as often as
-- indicated by 'ManyUpperBound'. The EpsilonFlag specifies whether succeeding
-- on empty input is permitted or not.
addCmdPartMany
  :: Typeable p
  => ManyUpperBound
  -> PartDesc
  -> (String -> EpsilonFlag -> Maybe (p, String))
  -> CmdParser out [p]
addCmdPartMany b p f =
  liftAp $ CmdParserPartManyInp b p (convertStringToInputParse f) id

-- | Add part that is expected to occur exactly once in the input.
-- The EpsilonFlag specifies whether succeeding on empty input is permitted
-- or not.
--
-- Only difference to 'addCmdPart' is that it accepts 'Input', i.e. can
-- behave differently for @String@ and @[String]@ input.
addCmdPartInp
  :: Typeable p
  => PartDesc
  -> (Input -> EpsilonFlag -> Maybe (p, Input))
  -> CmdParser out p
addCmdPartInp p f = liftAp $ CmdParserPartInp p f id

-- | Add part that is not required to occur, and can occur as often as
-- indicated by 'ManyUpperBound'.
-- The EpsilonFlag specifies whether succeeding on empty input is permitted
-- or not.
--
-- Only difference to 'addCmdPart' is that it accepts 'Input', i.e. can
-- behave differently for @String@ and @[String]@ input.
addCmdPartManyInp
  :: Typeable p
  => ManyUpperBound
  -> PartDesc
  -> (Input -> EpsilonFlag -> Maybe (p, Input))
  -> CmdParser out [p]
addCmdPartManyInp b p f = liftAp $ CmdParserPartManyInp b p f id

-- | Best explained via example:
--
-- > do
-- >   reorderStart
-- >   bright <- addSimpleBoolFlag "" ["bright"] mempty
-- >   yellow <- addSimpleBoolFlag "" ["yellow"] mempty
-- >   reorderStop
-- >   ..
--
-- will accept any inputs "" "--bright" "--yellow" "--bright --yellow" "--yellow --bright".
--
-- This works for any flags/params, but bear in mind that the results might
-- be unexpected because params may match on any input.
--
-- Note that start/stop must occur in pairs, and it will be a runtime error
-- if you mess this up. Use 'toCmdDesc' if you want to check all parts
-- of your 'CmdParser' without providing inputs that provide 100% coverage.
reorderStart :: CmdParser out ()
reorderStart = liftAp $ CmdParserReorderStart ()

-- | See 'reorderStart'
reorderStop :: CmdParser out ()
reorderStop = liftAp $ CmdParserReorderStop ()

-- | Add a new child command in the current context.
addCmd
  :: String -- ^ command name
  -> CmdParser out out -- ^ subcommand
  -> CmdParser out ()
addCmd str sub = liftAp $ CmdParserChild str Visible sub ()

-- | Add a new child command in the current context, but make it hidden. It
-- will not appear in docs/help generated by e.g. the functions in the
-- @Pretty@ module.
--
-- This feature is not well tested yet.
addCmdHidden
  :: String -- ^ command name
  -> CmdParser out out -- ^ subcommand
  -> CmdParser out ()
addCmdHidden str sub = liftAp $ CmdParserChild str Hidden sub ()


-- | Get the CommandDesc on the current level of the parser
-- (i.e. for a command child, you get the child's CommandDesc).
peekCmdDesc :: CmdParser out CommandDesc
peekCmdDesc = liftAp $ CmdParserPeekDesc id


extract :: (a -> Maybe b) -> [a] -> ([a], Maybe b)
extract _ []       = ([], Nothing)
extract f (x : xs) = case f x of
  Nothing -> let ~(l, m) = extract f xs in (x : l, m)
  Just b  -> (xs, Just b)

-- I don't believe this version is any more efficient. It _can_ be one tad
-- easier to use if it matches this pattern, but you _cannot_ get a non-strict
-- delete out of this any longer.
-- extractCont :: (a -> Maybe ([a] -> b)) -> [a] -> Maybe b
-- extractCont f = go id
--  where
--   go _         []       = Nothing
--   go startList (x : xs) = case f x of
--     Nothing -> go ((x :) . startList) xs
--     Just g  -> Just (g (startList xs))
