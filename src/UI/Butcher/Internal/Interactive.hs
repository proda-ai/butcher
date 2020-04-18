-- | Utilities when writing interactive programs that interpret commands,
-- e.g. a REPL.
module UI.Butcher.Internal.Interactive
  ( partDescStrings
  , CompletionItem(..)
  , PartialParseInfo(..)
  , combinedCompletion
  )
where



#include "prelude.inc"

import qualified Text.PrettyPrint              as PP

import           UI.Butcher.Internal.Monadic
import           UI.Butcher.Internal.MonadicTypes
import           UI.Butcher.Monadic.Pretty



combinedCompletion
  :: Input
  -> CommandDesc
  -> CommandDesc
  -> Input
  -> Either ParsingError (Maybe out)
  -> PartialParseInfo out
combinedCompletion line topDesc localDesc pcRest e = PartialParseInfo
  { _ppi_mainDesc        = topDesc
  , _ppi_localDesc       = localDesc
  , _ppi_value           = e
  , _ppi_line            = line
  , _ppi_rest            = pcRest
  , _ppi_lastword        = lastWord
  , _ppi_choices         = fst <$> choices
  , _ppi_choicesHelp     = choices
  , _ppi_choiceCommon    = longestCommonPrefix
  , _ppi_inputSugg       = compl
  , _ppi_prioDesc        = prioDesc
  , _ppi_interactiveHelp = interactiveHelp
  }
 where
  lastWord = case line of
    InputString s  -> reverse $ takeWhile (not . Char.isSpace) $ reverse s
    InputArgs   ss -> List.last ss
  nullRest = case pcRest of
    InputString s  -> null s
    InputArgs   ss -> null ss
  nameDesc = case _cmd_mParent localDesc of
    Nothing -> localDesc
    Just (_, parent) | nullRest && not (null lastWord) -> parent
        -- not finished writing a command. if we have commands abc and abcdef,
        -- we may want "def" as a completion after "abc".
    Just{}  -> localDesc
  choicesViaParent :: [(CompletionItem, Maybe String)] -- input, help
  choicesViaParent = join
    [ [ (CompletionString r, fmap show $ _cmd_synopsis c)
      | (Just r, c) <- Data.Foldable.toList (_cmd_children nameDesc)
      , lastWord `isPrefixOf` r
      -- , lastWord /= r
      ]
    , [ (CompletionString s, h) -- TODO we might not want to restrict to
                                -- CompletionString here
      | (CompletionString s, h) <- partDescComplsWithHelp Nothing
        =<< _cmd_parts nameDesc
      , lastWord `isPrefixOf` s
      -- , lastWord /= s
      ]
    ]
  prioDesc = case e of
    Left err -> _pe_expectedDesc err
    Right{}  -> Nothing
  choices = case prioDesc of
    Just d  -> partDescComplsWithHelp Nothing d
    Nothing -> choicesViaParent
  complStrs           = [ s | (CompletionString s, _) <- choices ]
  longestCommonPrefix = case complStrs of
    [] -> ""
    (c1 : cr) ->
      case
          find (\s -> List.all (s `isPrefixOf`) cr) $ reverse $ List.inits c1
        of
          Nothing -> ""
          Just x  -> x
  compl    = List.drop (List.length lastWord) longestCommonPrefix
  nullLine = case line of
    InputString "" -> True
    InputArgs   [] -> True
    _              -> False
  interactiveHelp maxLines = if
    | nullLine      -> helpStrShort
    | null lastWord -> helpStrShort
    | nullRest      -> helpStr maxLines
    | otherwise     -> helpStr maxLines
  helpStr maxLines = if List.length choices > maxLines
    then PP.fcat $ List.intersperse (PP.text "|") $ PP.text <$> complStrs
    else PP.vcat $ choices >>= \case
      (CompletionString s, Nothing) -> [PP.text s]
      (CompletionString s, Just h ) -> [PP.text s PP.<+> PP.text h]
      (_                 , Nothing) -> []
      (_                 , Just h ) -> [PP.text h]
  helpStrShort = ppUsageWithHelp localDesc


partDescComplsWithHelp
  :: Maybe String -> PartDesc -> [(CompletionItem, Maybe String)]
partDescComplsWithHelp mHelp = \case
  PartLiteral  s       -> [(CompletionString s, mHelp)]
  PartVariable _       -> []
  -- TODO: we could handle seq of optional and such much better
  PartOptional x       -> rec x
  PartAlts     alts    -> alts >>= rec
  PartSeq      []      -> []
  PartSeq      (x : _) -> rec x
  PartDefault    _  x  -> rec x
  PartSuggestion ss x  -> [ (c, mHelp) | c <- ss ] ++ rec x
  PartRedirect   _  x  -> rec x
  PartReorder xs       -> xs >>= rec
  PartMany    x        -> rec x
  PartWithHelp h x     -> partDescComplsWithHelp (Just $ show h) x
  PartHidden{}         -> []
  where rec = partDescComplsWithHelp mHelp


-- | Obtains a list of "expected"/potential strings for a command part
-- described in the 'PartDesc'. In constrast to the 'simpleCompletion'
-- function this function does not take into account any current input, and
-- consequently the output elements can in general not be appended to partial
-- input to form valid input.
partDescStrings :: PartDesc -> [String]
partDescStrings = \case
  PartLiteral  s       -> [s]
  PartVariable _       -> []
  -- TODO: we could handle seq of optional and such much better
  PartOptional x       -> partDescStrings x
  PartAlts     alts    -> alts >>= partDescStrings
  PartSeq      []      -> []
  PartSeq      (x : _) -> partDescStrings x
  PartDefault    _  x  -> partDescStrings x
  PartSuggestion ss x  -> [ s | CompletionString s <- ss ] ++ partDescStrings x
  PartRedirect   _  x  -> partDescStrings x
  PartReorder xs       -> xs >>= partDescStrings
  PartMany    x        -> partDescStrings x
  PartWithHelp _h x    -> partDescStrings x -- TODO: handle help
  PartHidden{}         -> []


-- | Obtains a list of "expected"/potential strings for a command part
-- described in the 'PartDesc'. In constrast to the 'simpleCompletion'
-- function this function does not take into account any current input, and
-- consequently the output elements can in general not be appended to partial
-- input to form valid input.
-- This is currently not properly implemented 
_partDescCompletions :: PartDesc -> [CompletionItem]
_partDescCompletions = fmap CompletionString . partDescStrings
