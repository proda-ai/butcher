{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module UI.Butcher.Internal.Monadic
  ( addCmdSynopsis
  , addCmdHelp
  , addCmdHelpStr
  , peekCmdDesc
  , peekInput
  , addCmdPart
  , addCmdPartA
  , addCmdPartMany
  , addCmdPartManyA
  , addCmdPartInp
  , addCmdPartInpA
  , addCmdPartManyInp
  , addCmdPartManyInpA
  , addCmd
  , addCmdHidden
  , addNullCmd
  , addCmdImpl
  , addAlternatives
  , reorderStart
  , reorderStop
  , toCmdDesc
  , traverseBarbie
  , runCmdParserCoreFromDesc
  , runCmdParserCoreFromDescA
  , mapOut
  , varPartDesc
  )
where



#include "prelude.inc"

import qualified Barbies
import qualified Barbies.Bare                  as Barbies
import           Control.Monad.Free
import qualified Control.Monad.Trans.MultiRWS.Strict
                                               as MultiRWSS
import qualified Control.Monad.Trans.MultiState.Strict
                                               as MultiStateS

import           Data.Monoid                    ( First(..) )
import qualified Lens.Micro                    as Lens
import           Lens.Micro                     ( (%~)
                                                , (.~)
                                                )

import qualified Text.PrettyPrint              as PP
import           Text.PrettyPrint               ( ($$)
                                                , ($+$)
                                                , (<+>)
                                                )

import           Data.HList.ContainsType

import           Data.Dynamic

import           UI.Butcher.Internal.MonadicTypes



-- general-purpose helpers
----------------------------

mModify :: MonadMultiState s m => (s -> s) -> m ()
mModify f = mGet >>= mSet . f

-- sadly, you need a degree in type inference to know when we can use
-- these operators and when it must be avoided due to type ambiguities
-- arising around s in the signatures below. That's the price of not having
-- the functional dependency in MonadMulti*T.

-- (.=+) :: MonadMultiState s m => Lens.ASetter s s a b -> b -> m ()
-- l .=+ b = mModify $ l .~ b
-- 
-- (%=+) :: MonadMultiState s m => Lens.ASetter s s a b -> (a -> b) -> m ()
-- l %=+ f = mModify (l %~ f)

-- inflateStateProxy :: (Monad m, ContainsType s ss)
--                   => p s -> StateS.StateT s m a -> MultiRWSS.MultiRWST r w ss m a
-- inflateStateProxy _ = MultiRWSS.inflateState

-- more on-topic stuff
----------------------------

-- instance IsHelpBuilder (CmdBuilder out) where
--   help s = liftF $ CmdBuilderHelp s ()
-- 
-- instance IsHelpBuilder (ParamBuilder p) where
--   help s = liftF $ ParamBuilderHelp s ()
-- 
-- instance IsHelpBuilder FlagBuilder where
--   help s = liftF $ FlagBuilderHelp s ()

-- | Add a synopsis to the command currently in scope; at top level this will
-- be the implicit top-level command.
--
-- Adding a second synopsis will overwrite a previous synopsis;
-- 'toCmdDesc' will check that you don't (accidentally) do this however.
addCmdSynopsis :: String -> CmdParser f out ()
addCmdSynopsis s = liftF $ CmdParserSynopsis s ()

-- | Add a help document to the command currently in scope; at top level this
-- will be the implicit top-level command.
--
-- Adding a second document will overwrite a previous document;
-- 'toCmdDesc' will check that you don't (accidentally) do this however.
addCmdHelp :: PP.Doc -> CmdParser f out ()
addCmdHelp s = liftF $ CmdParserHelp s ()

-- | Like @'addCmdHelp' . PP.text@
addCmdHelpStr :: String -> CmdParser f out ()
addCmdHelpStr s = liftF $ CmdParserHelp (PP.text s) ()

-- | Get the CommandDesc on the current level of the parser
-- (i.e. for a command child, you get the child's CommandDesc).
peekCmdDesc :: CmdParser f out CommandDesc
peekCmdDesc = liftF $ CmdParserPeekDesc id

-- | Semi-hacky way of accessing the current input that is not yet processed.
-- This must not be used to do any parsing. The purpose of this function is
-- to provide a String to be used for output to the user, as feedback about
-- what command was executed. For example we may think of an interactive
-- program reacting to commandline input such as
-- "run --delay 60 fire-rockets" which shows a 60 second delay on the
-- "fire-rockets" command. The latter string could have been obtained
-- via 'peekInput' after having parsed "run --delay 60" already.
peekInput :: CmdParser f out String
peekInput = liftF $ CmdParserPeekInput id

-- | Add part that is expected to occur exactly once in the input. May
-- succeed on empty input (e.g. by having a default).
addCmdPart
  :: (Applicative f, Typeable p)
  => PartDesc
  -> PartParser p String
  -> CmdParser f out p
addCmdPart p f = liftF $ CmdParserPart p f (\_ -> pure ()) id

addCmdPartA
  :: (Typeable p)
  => PartDesc
  -> PartParser p String
  -> (p -> f ())
  -> CmdParser f out p
addCmdPartA p f a = liftF $ CmdParserPart p f a id

-- | Add part that is not required to occur, and can occur as often as
-- indicated by 'ManyUpperBound'. Must not succeed on empty input.
addCmdPartMany
  :: (Applicative f, Typeable p)
  => ManyUpperBound
  -> PartDesc
  -> PartParser p String
  -> CmdParser f out [p]
addCmdPartMany b p f = liftF $ CmdParserPartMany b p f (\_ -> pure ()) id

addCmdPartManyA
  :: (Typeable p)
  => ManyUpperBound
  -> PartDesc
  -> PartParser p String
  -> (p -> f ())
  -> CmdParser f out [p]
addCmdPartManyA b p f a = liftF $ CmdParserPartMany b p f a id

-- | Add part that is expected to occur exactly once in the input. May
-- succeed on empty input (e.g. by having a default).
--
-- Only difference to 'addCmdPart' is that it accepts 'Input', i.e. can
-- behave differently for @String@ and @[String]@ input.
addCmdPartInp
  :: (Applicative f, Typeable p)
  => PartDesc
  -> PartParser p Input
  -> CmdParser f out p
addCmdPartInp p f = liftF $ CmdParserPartInp p f (\_ -> pure ()) id

addCmdPartInpA
  :: (Typeable p)
  => PartDesc
  -> PartParser p Input
  -> (p -> f ())
  -> CmdParser f out p
addCmdPartInpA p f a = liftF $ CmdParserPartInp p f a id

-- | Add part that is not required to occur, and can occur as often as
-- indicated by 'ManyUpperBound'. Must not succeed on empty input.
--
-- Only difference to 'addCmdPart' is that it accepts 'Input', i.e. can
-- behave differently for @String@ and @[String]@ input.
addCmdPartManyInp
  :: (Applicative f, Typeable p)
  => ManyUpperBound
  -> PartDesc
  -> PartParser p Input
  -> CmdParser f out [p]
addCmdPartManyInp b p f = liftF $ CmdParserPartManyInp b p f (\_ -> pure ()) id

addCmdPartManyInpA
  :: (Typeable p)
  => ManyUpperBound
  -> PartDesc
  -> PartParser p Input
  -> (p -> f ())
  -> CmdParser f out [p]
addCmdPartManyInpA b p f a = liftF $ CmdParserPartManyInp b p f a id

-- | Add a new child command in the current context.
addCmd
  :: Applicative f
  => String -- ^ command name
  -> CmdParser f out () -- ^ subcommand
  -> CmdParser f out ()
addCmd str sub = liftF $ CmdParserChild (Just str) Visible sub (pure ()) ()

-- | Add a new child command in the current context, but make it hidden. It
-- will not appear in docs/help generated by e.g. the functions in the
-- @Pretty@ module.
--
-- This feature is not well tested yet.
addCmdHidden
  :: Applicative f
  => String -- ^ command name
  -> CmdParser f out () -- ^ subcommand
  -> CmdParser f out ()
addCmdHidden str sub =
  liftF $ CmdParserChild (Just str) Hidden sub (pure ()) ()

-- | Add a list of sub-parsers one of which will be selected and used based
-- on the provided predicate function. The input elements consist of:
-- a) a name used for the command description of the output,
-- b) a predicate function; the first True predicate determines which element
--    to apply
-- c) a CmdParser.
addAlternatives
  :: Typeable p
  => [(String, String -> Bool, CmdParser f out p)]
  -> CmdParser f out p
addAlternatives elems = liftF $ CmdParserAlternatives desc alts id
 where
  desc = PartAlts $ [ PartVariable s | (s, _, _) <- elems ]
  alts = [ (a, b) | (_, a, b) <- elems ]

-- | Create a simple PartDesc from a string.
varPartDesc :: String -> PartDesc
varPartDesc = PartVariable

-- | Add a new nameless child command in the current context. Nameless means
-- that this command matches the empty input, i.e. will always apply.
-- This feature is experimental and CommandDesc pretty-printing might not
-- correctly in presense of nullCmds.
addNullCmd :: Applicative f => CmdParser f out () -> CmdParser f out ()
addNullCmd sub = liftF $ CmdParserChild Nothing Hidden sub (pure ()) ()

-- | Add an implementation to the current command.
addCmdImpl :: out -> CmdParser f out ()
addCmdImpl o = liftF $ CmdParserImpl o ()

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
reorderStart :: CmdParser f out ()
reorderStart = liftF $ CmdParserReorderStart ()

-- | See 'reorderStart'
reorderStop :: CmdParser f out ()
reorderStop = liftF $ CmdParserReorderStop ()

-- | If you have a higher-kinded config type (let's assume it is a plain
-- record) then this turns a record whose fields are @CmdParser@s over
-- different values into a CmdParser that returns a record with the parsed
-- values in the fields.
--
-- See the BarbieParsing example included in this package.
traverseBarbie
  :: (Barbies.BareB c, Barbies.TraversableB (c Barbies.Covered))
  => c Barbies.Covered (CmdParser f out)
  -> CmdParser f out (c Barbies.Bare Identity)
traverseBarbie k = do
  r <- Barbies.btraverse (fmap Identity) k
  pure $ Barbies.bstrip r


-- addPartHelp :: String -> CmdPartParser ()
-- addPartHelp s = liftF $ CmdPartParserHelp s ()
-- 
-- addPartParserBasic :: (String -> Maybe (p, String)) -> Maybe p -> CmdPartParser p
-- addPartParserBasic f def = liftF $ CmdPartParserCore f def id
-- 
-- addPartParserOptionalBasic :: CmdPartParser p -> CmdPartParser (Maybe p)
-- addPartParserOptionalBasic p = liftF $ CmdPartParserOptional p id

data PartGatherData f = forall p . Typeable p => PartGatherData
  { _pgd_id     :: Int
  , _pgd_desc   :: PartDesc
  , _pgd_parseF :: Either (PartParser p String) (PartParser p Input)
  , _pgd_act    :: p -> f ()
  , _pgd_many   :: Bool
  }

type PartParsedData = Map Int [Dynamic]

data CmdDescStack = StackBottom (Deque PartDesc)
                  | StackLayer  (Deque PartDesc) String CmdDescStack
  deriving Show

descStackAdd :: PartDesc -> CmdDescStack -> CmdDescStack
descStackAdd d = \case
  StackBottom l    -> StackBottom $ Deque.snoc d l
  StackLayer l s u -> StackLayer (Deque.snoc d l) s u


-- | Because butcher is evil (i.e. has constraints not encoded in the types;
-- see the README), this method can be used as a rough check that you did not
-- mess up. It traverses all possible parts of the 'CmdParser' thereby
-- ensuring that the 'CmdParser' has a valid structure.
--
-- This method also yields a _complete_ @CommandDesc@ output, where the other
-- runCmdParser* functions all traverse only a shallow structure around the
-- parts of the 'CmdParser' touched while parsing the current input.
toCmdDesc
  :: forall f out
   . Maybe String -- ^ top-level command name
  -> CmdParser f out () -- ^ parser to check
  -> Either String CommandDesc
toCmdDesc mTopLevel cmdParser =
  (>>= final)
    $ MultiRWSS.runMultiRWSTNil
    $ MultiRWSS.withMultiStateAS (StackBottom mempty)
    $ MultiRWSS.withMultiStateS emptyCommandDesc
    $ processMain cmdParser
 where
  final :: (CommandDesc, CmdDescStack) -> Either String CommandDesc
  final (desc, stack) = case stack of
    StackBottom descs ->
      Right
        $ descFixParentsWithTopM
            (mTopLevel <&> \n -> (Just n, emptyCommandDesc))
        $ desc { _cmd_parts = Data.Foldable.toList descs }
    StackLayer _ _ _ -> Left "unclosed ReorderStart or GroupStart"
  processMain
    :: CmdParser f out a
    -> MultiRWSS.MultiRWST
         '[]
         '[]
         '[CommandDesc , CmdDescStack]
         (Either String)
         a
  processMain = \case
    Pure x                      -> return x
    Free (CmdParserHelp h next) -> do
      cmd :: CommandDesc <- mGet
      mSet $ cmd { _cmd_help = Just h }
      processMain next
    Free (CmdParserSynopsis s next) -> do
      cmd :: CommandDesc <- mGet
      mSet
        $ cmd { _cmd_synopsis = Just $ PP.fsep $ fmap PP.text $ List.words s }
      processMain next
    Free (CmdParserPeekDesc nextF) -> do
      processMain $ nextF monadMisuseError
    Free (CmdParserPeekInput nextF) -> do
      processMain $ nextF monadMisuseError
    Free (CmdParserPart desc _parseF _act nextF) -> do
      do
        descStack <- mGet
        mSet $ descStackAdd desc descStack
      processMain $ nextF monadMisuseError
    Free (CmdParserPartInp desc _parseF _act nextF) -> do
      do
        descStack <- mGet
        mSet $ descStackAdd desc descStack
      processMain $ nextF monadMisuseError
    Free (CmdParserPartMany bound desc _parseF _act nextF) -> do
      do
        descStack <- mGet
        mSet $ descStackAdd (wrapBoundDesc bound desc) descStack
      processMain $ nextF monadMisuseError
    Free (CmdParserPartManyInp bound desc _parseF _act nextF) -> do
      do
        descStack <- mGet
        mSet $ descStackAdd (wrapBoundDesc bound desc) descStack
      processMain $ nextF monadMisuseError
    Free (CmdParserChild cmdStr vis sub _act next) -> do
      mInitialDesc       <- takeCommandChild cmdStr
      cmd :: CommandDesc <- mGet
      subCmd             <- do
        stackCur :: CmdDescStack <- mGet
        mSet $ Maybe.fromMaybe (emptyCommandDesc :: CommandDesc) mInitialDesc
        mSet $ StackBottom mempty
        processMain sub
        c          <- mGet
        stackBelow <- mGet
        mSet cmd
        mSet stackCur
        subParts <- case stackBelow of
          StackBottom descs -> return $ Data.Foldable.toList descs
          StackLayer _ _ _  -> lift $ Left "unclosed ReorderStart or GroupStart"
        return c { _cmd_parts = subParts, _cmd_visibility = vis }
      mSet $ cmd
        { _cmd_children = (cmdStr, subCmd) `Deque.snoc` _cmd_children cmd
        }
      processMain next
    Free (CmdParserImpl _out next) -> do
      -- no need to process _out when we just construct the CommandDesc.
      -- it would be full of monadmisuse-errors anyway.
      processMain $ next
    Free (CmdParserGrouped groupName next) -> do
      stackCur <- mGet
      mSet $ StackLayer mempty groupName stackCur
      processMain $ next
    Free (CmdParserGroupEnd next) -> do
      stackCur <- mGet
      case stackCur of
        StackBottom{} -> do
          lift $ Left $ "butcher interface error: group end without group start"
        StackLayer _descs "" _up -> do
          lift $ Left $ "GroupEnd found, but expected ReorderStop first"
        StackLayer descs groupName up -> do
          mSet $ descStackAdd
            (PartRedirect groupName (PartSeq (Data.Foldable.toList descs)))
            up
          processMain $ next
    Free (CmdParserReorderStop next) -> do
      stackCur <- mGet
      case stackCur of
        StackBottom{} -> lift $ Left $ "ReorderStop without reorderStart"
        StackLayer descs "" up -> do
          mSet $ descStackAdd (PartReorder (Data.Foldable.toList descs)) up
        StackLayer{} ->
          lift $ Left $ "Found ReorderStop, but need GroupEnd first"
      processMain next
    Free (CmdParserReorderStart next) -> do
      stackCur <- mGet
      mSet $ StackLayer mempty "" stackCur
      processMain next
    Free (CmdParserAlternatives desc alts nextF) -> do
      mModify (descStackAdd desc)
      states <- MultiRWSS.mGetRawS
      let go
            :: [(String -> Bool, CmdParser f out p)]
            -> MultiRWSS.MultiRWST
                 '[]
                 '[]
                 '[CommandDesc , CmdDescStack]
                 (Either String)
                 p
          go []                 = lift $ Left $ "Empty alternatives"
          go [(_, alt)        ] = processMain alt
          go ((_, alt1) : altr) = do
            case
                MultiRWSS.runMultiRWSTNil
                  $ MultiRWSS.withMultiStates states (processMain alt1)
              of
                Left{}             -> go altr
                Right (p, states') -> MultiRWSS.mPutRawS states' $> p
      p <- go alts
      processMain $ nextF p

  monadMisuseError :: a
  monadMisuseError =
    error
      $  "CmdParser definition error -"
      ++ " used Monad powers where only Applicative/Arrow is allowed"


data CoreInterpreterState f out = CoreInterpreterState
  { _cis_remainingInput   :: Input
  , _cis_pastCommandInput :: Input
  , _cis_output           :: Maybe out
  , _cis_currentParser    :: CmdParser f out ()
  , _cis_currentDesc      :: CommandDesc
  , _cis_expectedPartDesc :: Maybe PartDesc
  }


-- | Run a @CmdParser@ on the given input, returning:
--
-- a) A @CommandDesc ()@ that accurately represents the subcommand that was
--    reached, even if parsing failed. Because this is returned always, the
--    argument is @()@ because "out" requires a successful parse.
--
-- b) The remaining input, i.e. the left-over part that did not parse
--    successfully.
--    For some input "myprog foo bar -v --wrong" where parsing fails at
--    "--wrong", this will contain the full "-v --wrong". Useful for
--    interactive feedback stuff.

-- c) Either an error or the result of a successful parse, including a proper
--    "CommandDesc out" from which an "out" can be extracted (presuming that
--    the command has an implementation).
runCmdParserCoreFromDesc
  :: CommandDesc -- ^ cached desc
  -> Input -- ^ input to be processed
  -> CmdParser Identity out () -- ^ parser to use
  -> (CommandDesc, Input, Either ParsingError (Maybe out))
runCmdParserCoreFromDesc topDesc inputInitial cmdParser =
  runIdentity $ runCmdParserCoreFromDescA topDesc inputInitial cmdParser

-- | The Applicative-enabled version of 'runCmdParserCoreFromDesc'.
runCmdParserCoreFromDescA
  :: forall f out
   . Applicative f
  => CommandDesc -- ^ cached desc
  -> Input -- ^ input to be processed
  -> CmdParser f out () -- ^ parser to use
  -> f (CommandDesc, Input, Either ParsingError (Maybe out))
runCmdParserCoreFromDescA topDesc inputInitial cmdParser =
  runIdentity
    $ MultiRWSS.runMultiRWSTNil
    $ fmap captureFinal
    $ MultiRWSS.withMultiWriterWA
    $ MultiRWSS.withMultiStateSA initialState
    $ processMain cmdParser
 where
  initialState = CoreInterpreterState { _cis_remainingInput   = inputInitial
                                      , _cis_pastCommandInput = inputInitial
                                      , _cis_output           = Nothing
                                      , _cis_currentParser    = cmdParser
                                      , _cis_currentDesc      = topDesc
                                      , _cis_expectedPartDesc = Nothing
                                      }
  captureFinal
    :: ([String], (CoreInterpreterState f out, f ()))
    -> f (CommandDesc, Input, Either ParsingError (Maybe out))
  captureFinal (errs, (finalState, act)) =
    act $> (_cis_currentDesc finalState, _cis_pastCommandInput finalState, res)
   where
    errs'     = errs ++ inputErrs
    inputErrs = case _cis_remainingInput finalState of
      InputString s | all Char.isSpace s -> []
      InputString{} -> ["could not parse input/unprocessed input"]
      InputArgs [] -> []
      InputArgs{} -> ["could not parse input/unprocessed input"]
    res = if null errs'
      then Right (_cis_output finalState)
      else Left $ ParsingError
        { _pe_messages     = errs'
        , _pe_remaining    = _cis_remainingInput finalState
        , _pe_expectedDesc = _cis_expectedPartDesc finalState
        }
  processMain
    :: -- forall a
       CmdParser f out ()
    -> MultiRWSS.MultiRWS
         '[]
         '[[String]]
         '[CoreInterpreterState f out]
         (f ())
  processMain = \case
    Pure ()                      -> return $ pure ()
    Free (CmdParserHelp _h next) -> do
      processMain next
    Free (CmdParserSynopsis _s next) -> do
      processMain next
    Free (CmdParserPeekDesc nextF) -> do
      cis :: CoreInterpreterState f out <- mGet
      processMain $ nextF (_cis_currentDesc cis)
    Free (CmdParserPeekInput nextF) -> do
      processMain $ nextF $ inputToString inputInitial
    Free (CmdParserPart desc parseF actF nextF) -> do
      cis :: CoreInterpreterState f out <- mGet
      case _cis_remainingInput cis of
        InputString str -> case parseF str of
          Success x rest -> do
            mSet $ cis { _cis_remainingInput = InputString rest }
            actRest <- processMain $ nextF x
            return $ actF x *> actRest
          Failure errPDesc -> do
            mTell ["could not parse " ++ getPartSeqDescPositionName desc]
            trySetErrDesc errPDesc
            processMain $ nextF monadMisuseError
        InputArgs (str : strr) -> case parseF str of
          Success x "" -> do
            mSet $ cis { _cis_remainingInput = InputArgs strr }
            actRest <- processMain $ nextF x
            return $ actF x *> actRest
          Success x rest | str == rest -> do
            -- no input consumed, default applied
            actRest <- processMain $ nextF x
            return $ actF x *> actRest
          Success{} -> do
            mTell ["could not parse " ++ getPartSeqDescPositionName desc]
            processMain $ nextF monadMisuseError
          Failure errPDesc -> do
            mTell ["could not parse " ++ getPartSeqDescPositionName desc]
            trySetErrDesc errPDesc
            processMain $ nextF monadMisuseError
        InputArgs [] -> do
          mTell ["could not parse " ++ getPartSeqDescPositionName desc]
          processMain $ nextF monadMisuseError
    Free (CmdParserPartInp desc parseF actF nextF) -> do
      cis :: CoreInterpreterState f out <- mGet
      case parseF (_cis_remainingInput cis) of
        Success x rest -> do
          mSet $ cis { _cis_remainingInput = rest }
          actRest <- processMain $ nextF x
          return $ actF x *> actRest
        Failure errPDesc -> do
          mTell ["could not parse " ++ getPartSeqDescPositionName desc]
          trySetErrDesc errPDesc
          processMain $ nextF monadMisuseError
    Free (CmdParserPartMany _bound _desc parseF actF nextF) -> do
      let proc = do
            dropSpaces
            cis :: CoreInterpreterState f out <- mGet
            case _cis_remainingInput cis of
              InputString str -> case parseF str of
                Success x r -> do
                  mSet $ cis { _cis_remainingInput = InputString r }
                  xr <- proc
                  return $ x : xr
                Failure errPDesc -> do
                  trySetErrDesc errPDesc
                  return []
              InputArgs (str : strr) -> case parseF str of
                Success x "" -> do
                  mSet $ cis { _cis_remainingInput = InputArgs strr }
                  xr <- proc
                  return $ x : xr
                Success{} -> do
                  return []
                Failure errPDesc -> do
                  trySetErrDesc errPDesc
                  return []
              InputArgs [] -> return []
      r <- proc
      let act = traverse actF r
      (act *>) <$> processMain (nextF $ r)
    Free (CmdParserPartManyInp _bound _desc parseF actF nextF) -> do
      let proc = do
            dropSpaces
            cis :: CoreInterpreterState f out <- mGet
            case parseF (_cis_remainingInput cis) of
              Success x r -> do
                mSet $ cis { _cis_remainingInput = r }
                xr <- proc
                return $ x : xr
              Failure errPDesc -> do
                trySetErrDesc errPDesc
                return []
      r <- proc
      let act = traverse actF r
      (act *>) <$> processMain (nextF $ r)
    Free (CmdParserChild mName _vis inner act next) -> do
      dropSpaces
      input <- mGet @(CoreInterpreterState f out) <&> _cis_remainingInput
      let mRest = case (mName, input) of
            (Just name, InputString str) | name == str ->
              Just $ (Just name, InputString "")
            (Just name, InputString str) | (name ++ " ") `isPrefixOf` str ->
              Just $ (Just name, InputString $ drop (length name + 1) str)
            (Just name, InputArgs (str : strr)) | name == str ->
              Just $ (Just name, InputArgs strr)
            (Nothing, _) -> Just $ (Nothing, input)
            _            -> Nothing
      case mRest of
        Nothing -> do -- a child not matching what we have in the input
          -- get the shallow desc for the child in a separate env.
          -- proceed regularly on the same layer
          processMain next
        Just (name, rest) -> do -- matching child -> descend
          -- do the descend
          mModify $ \cis -> cis
            { _cis_remainingInput   = rest
            , _cis_pastCommandInput = rest
            , _cis_currentDesc      =
              case
                List.find
                  (\(n, _) -> name == n)
                  (Data.Foldable.toList $ _cmd_children $ _cis_currentDesc cis)
              of
                Nothing ->
                  error "butcher internal error: inconsistent child desc"
                Just (_, childDesc) -> childDesc
            , _cis_currentParser    = inner
            }
          childAct <- processMain inner
          -- check that descending yielded
          return $ act *> childAct
    Free (CmdParserImpl out next) -> do
      mModify @(CoreInterpreterState f out)
        $ \cis -> cis { _cis_output = Just out }
      processMain $ next
    Free (CmdParserGrouped _groupName next) -> do
      processMain $ next
    Free (CmdParserGroupEnd next) -> do
      processMain $ next
    Free (CmdParserReorderStop next) -> do
      mTell $ ["butcher interface error: reorder stop without reorder start"]
      processMain next
    Free (CmdParserReorderStart next) -> do
      reorderData <-
        MultiRWSS.withMultiStateA (1 :: Int)
        $ MultiRWSS.withMultiWriterW
        $ iterM reorderPartGather
        $ next
      let
        reorderMapInit :: Map Int (PartGatherData f)
        reorderMapInit = MapS.fromList $ reorderData <&> \d -> (_pgd_id d, d)
        tryParsePartData
          :: Input
          -> PartGatherData f
          -> First (Either PartDesc (Int, Dynamic, Input, Bool, f ()))
        tryParsePartData input (PartGatherData pid _ pfe act allowMany) =
          case pfe of
            Left pfStr -> case input of
              InputString str -> case pfStr str of
                Success x r | r /= str ->
                  pure $ Right (pid, toDyn x, InputString r, allowMany, act x)
                Failure (Just pDesc) -> pure $ Left pDesc
                _                    -> mempty
              InputArgs (str : strr) -> case pfStr str of
                Success x "" ->
                  pure $ Right (pid, toDyn x, InputArgs strr, allowMany, act x)
                Failure (Just pDesc) -> pure $ Left pDesc
                _                    -> First Nothing
              InputArgs [] -> First Nothing
            Right pfInp -> case pfInp input of
              Success x r | r /= input ->
                pure $ Right (pid, toDyn x, r, allowMany, act x)
              Failure (Just pDesc) -> pure $ Left pDesc
              _                    -> First Nothing

          -- First
          -- [ (pid, toDyn r, rest, allowMany, act r)
          --  | (r, rest) <- 
          -- ]
        parseLoop = do
          cis :: CoreInterpreterState f out <- mGet
          m :: Map Int (PartGatherData f)   <- mGet
          case
              getFirst $ Data.Foldable.foldMap
                (tryParsePartData $ _cis_remainingInput cis)
                m
            of
                     -- i will be angry if foldMap ever decides to not fold
                     -- in order of keys.
              Nothing -> return $ pure ()
              Just (Right (pid, x, rest, more, act)) -> do
                mSet cis { _cis_remainingInput = rest }
                mModify $ MapS.insertWith (++) pid [x]
                when (not more) $ do
                  mSet $ MapS.delete pid m
                actRest <- parseLoop
                return $ act *> actRest
              Just (Left err) -> do
                trySetErrDesc (Just err)
                return $ pure ()
      (finalMap, (fr, acts)) <-
        MultiRWSS.withMultiStateSA (MapS.empty :: PartParsedData)
        $ MultiRWSS.withMultiStateA reorderMapInit
        $ do
            acts <- parseLoop -- filling the map
            fr <- MultiRWSS.withMultiStateA (1 :: Int) $ processParsedParts next
            return (fr, acts)
      -- we check that all data placed in the map has been consumed while
      -- running the parts for which we collected the parseresults.
      -- there can only be any rest if the collection of parts changed
      -- between the reorderPartGather traversal and the processParsedParts
      -- consumption.
      if MapS.null finalMap
        then do
          actRest <- processMain fr
          return $ acts *> actRest
        else monadMisuseError
    Free (CmdParserAlternatives desc alts nextF) -> do
      cis :: CoreInterpreterState f out <- mGet
      case _cis_remainingInput cis of
        InputString str
          | Just (_, sub) <- find (\(predicate, _sub) -> predicate str) alts
          -> processMain $ sub >>= nextF
        InputArgs (str : _)
          | Just (_, sub) <- find (\(predicate, _sub) -> predicate str) alts
          -> processMain $ sub >>= nextF
        _ -> do
          mTell ["could not parse any of " ++ getPartSeqDescPositionName desc]
          processMain $ nextF monadMisuseError

  trySetErrDesc
    :: (MonadMultiState (CoreInterpreterState f out) m)
    => Maybe PartDesc
    -> m ()
  trySetErrDesc errPDescMay = do
    mModify $ \(cis :: CoreInterpreterState f out) -> cis
      { _cis_expectedPartDesc = _cis_expectedPartDesc cis <|> errPDescMay
      }
  reorderPartGather
    :: ( MonadMultiState Int m
       , MonadMultiWriter [PartGatherData f] m
       , MonadMultiWriter [String] m
       )
    => CmdParserF f out (m ())
    -> m ()
  reorderPartGather = \case
    -- TODO: why do PartGatherData contain desc?
    CmdParserPart desc parseF actF nextF -> do
      pid <- mGet
      mSet $ pid + 1
      mTell [PartGatherData pid desc (Left parseF) actF False]
      nextF $ monadMisuseError
    CmdParserPartInp desc parseF actF nextF -> do
      pid <- mGet
      mSet $ pid + 1
      mTell [PartGatherData pid desc (Right parseF) actF False]
      nextF $ monadMisuseError
    CmdParserPartMany _ desc parseF actF nextF -> do
      pid <- mGet
      mSet $ pid + 1
      mTell [PartGatherData pid desc (Left parseF) actF True]
      nextF $ monadMisuseError
    CmdParserPartManyInp _ desc parseF actF nextF -> do
      pid <- mGet
      mSet $ pid + 1
      mTell [PartGatherData pid desc (Right parseF) actF True]
      nextF $ monadMisuseError
    CmdParserReorderStop _next -> do
      return ()
    CmdParserHelp{}         -> restCase
    CmdParserSynopsis{}     -> restCase
    CmdParserPeekDesc{}     -> restCase
    CmdParserPeekInput{}    -> restCase
    CmdParserChild{}        -> restCase
    CmdParserImpl{}         -> restCase
    CmdParserReorderStart{} -> restCase
    CmdParserGrouped{}      -> restCase
    CmdParserGroupEnd{}     -> restCase
    CmdParserAlternatives{} -> restCase
   where
    restCase = do
      mTell ["Did not find expected ReorderStop after the reordered parts"]
      return ()

  processParsedParts
    :: forall m r w s m0 a
     . ( MonadMultiState Int m
       , MonadMultiState PartParsedData m
       , MonadMultiState (Map Int (PartGatherData f)) m
       , MonadMultiState (CoreInterpreterState f out) m
       , MonadMultiWriter [[Char]] m
       -- , ContainsType (CoreInterpreterState f out) s
       , m ~ MultiRWSS.MultiRWST r w s m0
       , Monad m0
       )
    => CmdParser f out a
    -> m (CmdParser f out a)
  processParsedParts = \case
    Free (CmdParserPart desc _ _ (nextF :: p -> CmdParser f out a)) ->
      part desc nextF
    Free (CmdParserPartInp desc _ _ (nextF :: p -> CmdParser f out a)) ->
      part desc nextF
    Free (CmdParserPartMany bound desc _ _ nextF) -> partMany bound desc nextF
    Free (CmdParserPartManyInp bound desc _ _ nextF) ->
      partMany bound desc nextF
    Free (CmdParserReorderStop next) -> do
      return next
    Free (CmdParserGrouped _groupName next) -> do
      processParsedParts $ next
    Free (CmdParserGroupEnd next) -> do
      processParsedParts $ next
    Pure x -> return $ return $ x
    f      -> do
      mTell ["Did not find expected ReorderStop after the reordered parts"]
      return f
   where
    part
      :: forall p
       . Typeable p
      => PartDesc
      -> (p -> CmdParser f out a)
      -> m (CmdParser f out a)
    part desc nextF = do
      pid <- mGet
      mSet $ pid + 1
      parsedMap :: PartParsedData <- mGet
      mSet $ MapS.delete pid parsedMap
      partMap :: Map Int (PartGatherData f) <- mGet
      cis :: CoreInterpreterState f out     <- mGet
      let
        errorResult = do
          mTell
            [ "could not parse expected input "
              ++ getPartSeqDescPositionName desc
              ++ " with remaining input: "
              ++ show (_cis_remainingInput cis)
            ]
          processParsedParts $ nextF monadMisuseError
        continueOrMisuse :: Maybe p -> m (CmdParser f out a)
        continueOrMisuse = maybe monadMisuseError (processParsedParts . nextF)
      case MapS.lookup pid parsedMap of
        Nothing -> case MapS.lookup pid partMap of
          Nothing                           -> monadMisuseError -- it would still be in the map
                                      -- if it never had been successfully
                                      -- parsed, as indicicated by the
                                      -- previous parsedMap Nothing lookup.
          Just (PartGatherData _ _ pfe _ _) -> case pfe of
            Left pf -> case pf "" of
              Success dx _ -> continueOrMisuse $ cast dx
              Failure _    -> errorResult
            Right pf -> case pf (InputArgs []) of
              Success dx _ -> continueOrMisuse $ cast dx
              Failure _    -> errorResult
        Just [dx] -> continueOrMisuse $ fromDynamic dx
        Just _    -> monadMisuseError
    partMany
      :: Typeable p
      => ManyUpperBound
      -> PartDesc
      -> ([p] -> CmdParser f out a)
      -> m (CmdParser f out a)
    partMany _bound _desc nextF = do
      pid <- mGet
      mSet $ pid + 1
      m :: PartParsedData <- mGet
      mSet $ MapS.delete pid m
      let partDyns = case MapS.lookup pid m of
            Nothing -> []
            Just r  -> reverse r
      case mapM fromDynamic partDyns of
        Nothing -> monadMisuseError
        Just xs -> processParsedParts $ nextF xs

  -- postProcessCmd :: CmdDescStack -> CommandDesc out -> CommandDesc out
  -- postProcessCmd descStack cmd = descFixParents $ cmd
  --   { _cmd_parts = case descStack of
  --     StackBottom l -> Data.Foldable.toList l
  --     StackLayer{}  -> []
  --   }

  monadMisuseError :: a
  monadMisuseError =
    error
      $  "CmdParser definition error -"
      ++ " used Monad powers where only Applicative/Arrow is allowed"


  getPartSeqDescPositionName :: PartDesc -> String
  getPartSeqDescPositionName = \case
    PartLiteral  s     -> s
    PartVariable s     -> s
    PartOptional ds'   -> f ds'
    PartAlts     alts  -> f $ head alts -- this is not optimal, but probably
                                   -- does not matter.
    PartDefault    _ d -> f d
    PartSuggestion _ d -> f d
    PartRedirect   s _ -> s
    PartMany ds        -> f ds
    PartWithHelp _ d   -> f d
    PartSeq     ds     -> List.unwords $ f <$> ds
    PartReorder ds     -> List.unwords $ f <$> ds
    PartHidden  d      -> f d
    where f = getPartSeqDescPositionName

  dropSpaces :: MonadMultiState (CoreInterpreterState f out) m => m ()
  dropSpaces = do
    cis :: CoreInterpreterState f out <- mGet
    case _cis_remainingInput cis of
      InputString s -> mSet
        $ cis { _cis_remainingInput = InputString $ dropWhile Char.isSpace s }
      InputArgs{} -> return ()

  inputToString :: Input -> String
  inputToString (InputString s ) = s
  inputToString (InputArgs   ss) = List.unwords ss

dequeLookupRemove :: Eq k => k -> Deque (k, a) -> (Maybe a, Deque (k, a))
dequeLookupRemove key deque = case Deque.uncons deque of
  Nothing             -> (Nothing, mempty)
  Just ((k, v), rest) -> if k == key
    then (Just v, rest)
    else
      let (r, rest') = dequeLookupRemove key rest
      in  (r, Deque.cons (k, v) rest')

takeCommandChild
  :: MonadMultiState CommandDesc m => Maybe String -> m (Maybe CommandDesc)
takeCommandChild key = do
  cmd <- mGet
  let (r, children') = dequeLookupRemove key $ _cmd_children cmd
  mSet cmd { _cmd_children = children' }
  return r

-- | map over the @out@ type argument
mapOut :: (outa -> outb) -> CmdParser f outa a -> CmdParser f outb a
mapOut f = hoistFree $ \case
  CmdParserHelp     doc r     -> CmdParserHelp doc r
  CmdParserSynopsis s   r     -> CmdParserSynopsis s r
  CmdParserPeekDesc  fr       -> CmdParserPeekDesc fr
  CmdParserPeekInput fr       -> CmdParserPeekInput fr
  CmdParserPart desc fp fa fr -> CmdParserPart desc fp fa fr
  CmdParserPartMany bound desc fp fa fr ->
    CmdParserPartMany bound desc fp fa fr
  CmdParserPartInp desc fp fa fr -> CmdParserPartInp desc fp fa fr
  CmdParserPartManyInp bound desc fp fa fr ->
    CmdParserPartManyInp bound desc fp fa fr
  CmdParserChild s vis child act r ->
    CmdParserChild s vis (mapOut f child) act r
  CmdParserImpl out r               -> CmdParserImpl (f out) r
  CmdParserReorderStart r           -> CmdParserReorderStart r
  CmdParserReorderStop  r           -> CmdParserReorderStop r
  CmdParserGrouped s r              -> CmdParserGrouped s r
  CmdParserGroupEnd r               -> CmdParserGroupEnd r
  CmdParserAlternatives desc alts r -> CmdParserAlternatives
    desc
    [ (predicate, mapOut f sub) | (predicate, sub) <- alts ]
    r

-- cmdActionPartial :: CommandDesc out -> Either String out
-- cmdActionPartial = maybe (Left err) Right . _cmd_out
--   where
--     err = "command is missing implementation!"
--  
-- cmdAction :: CmdParser out () -> String -> Either String out
-- cmdAction b s = case runCmdParser Nothing s b of
--   (_, Right cmd)                     -> cmdActionPartial cmd
--   (_, Left (ParsingError (out:_) _)) -> Left $ out
--   _ -> error "whoops"
-- 
-- cmdActionRun :: (CommandDesc () -> ParsingError -> out)
--              -> CmdParser out ()
--              -> String
--              -> out
-- cmdActionRun f p s = case runCmdParser Nothing s p of
--   (cmd, Right out) -> case _cmd_out out of
--     Just o -> o
--     Nothing -> f cmd (ParsingError ["command is missing implementation!"] "")
--   (cmd, Left err) -> f cmd err

wrapBoundDesc :: ManyUpperBound -> PartDesc -> PartDesc
wrapBoundDesc ManyUpperBound1 = PartOptional
wrapBoundDesc ManyUpperBoundN = PartMany


_descFixParents :: CommandDesc -> CommandDesc
_descFixParents = descFixParentsWithTopM Nothing

-- descFixParentsWithTop :: String -> CommandDesc a -> CommandDesc a
-- descFixParentsWithTop s = descFixParentsWithTopM (Just (s, emptyCommandDesc))

descFixParentsWithTopM
  :: Maybe (Maybe String, CommandDesc) -> CommandDesc -> CommandDesc
descFixParentsWithTopM mTop topDesc = Data.Function.fix $ \fixed -> topDesc
  { _cmd_mParent  = goUp fixed <$> (mTop <|> _cmd_mParent topDesc)
  , _cmd_children = _cmd_children topDesc <&> goDown fixed
  }
 where
  goUp
    :: CommandDesc -> (Maybe String, CommandDesc) -> (Maybe String, CommandDesc)
  goUp child (childName, parent) =
    (,) childName $ Data.Function.fix $ \fixed -> parent
      { _cmd_mParent  = goUp fixed <$> _cmd_mParent parent
      , _cmd_children = _cmd_children parent <&> \(n, c) ->
                          if n == childName then (n, child) else (n, c)
      }
  goDown
    :: CommandDesc -> (Maybe String, CommandDesc) -> (Maybe String, CommandDesc)
  goDown parent (childName, child) =
    (,) childName $ Data.Function.fix $ \fixed -> child
      { _cmd_mParent  = Just (childName, parent)
      , _cmd_children = _cmd_children child <&> goDown fixed
      }


_tooLongText
  :: Int -- max length
  -> String -- alternative if actual length is bigger than max.
  -> String -- text to print, if length is fine.
  -> PP.Doc
_tooLongText i alt s = PP.text $ Bool.bool alt s $ null $ drop i s
