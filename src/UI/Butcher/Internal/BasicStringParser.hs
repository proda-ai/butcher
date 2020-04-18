{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module UI.Butcher.Internal.BasicStringParser
  ( InpParseString(..)
  , runInpParseString
  , pExpect
  , pExpectEof
  , pDropSpace
  , pOption
  )
where



#include "prelude.inc"



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

pDropSpace :: InpParseString ()
pDropSpace = InpParseString $ StateS.modify (dropWhile (== ' '))

pOption :: InpParseString () -> InpParseString ()
pOption m = m <|> return ()

