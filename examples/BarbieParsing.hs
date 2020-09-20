{-# LANGUAGE DeriveGeneric #-}

module Main where

import           Barbies
import           Barbies.Bare
import           GHC.Generics                   ( Generic )
import           UI.Butcher.Monadic



data MyConfig s f = MyConfig
  { verbosity    :: Wear s f Int
  , dryRun       :: Wear s f Bool
  , innerOptions :: Wear s f [String]
  }
  deriving Generic

instance BareB MyConfig
instance FunctorB (MyConfig Covered)
instance TraversableB (MyConfig Covered)

main :: IO ()
main = mainFromCmdParser $ do

  reorderStart
  config <- traverseBarbie MyConfig
    { verbosity    = addFlagReadParam "v" ["verbosity"] "INT" (flagDefault 1)
    , dryRun       = addSimpleBoolFlag "" ["dryRun", "dry-run"] mempty
    , innerOptions = addFlagStringParams "" ["inner-option"] "OPT" mempty
    }
  reorderStop

  addCmdImpl $ do
    putStrLn $ "commandline arguments produced the following config values:"
    putStrLn $ "verbosity    = " ++ show (verbosity config)
    putStrLn $ "dryRun       = " ++ show (dryRun config)
    putStrLn $ "innerOptions = " ++ show (innerOptions config)

