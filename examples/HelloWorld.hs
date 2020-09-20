module Main where

import           UI.Butcher.Monadic



main :: IO ()
main = mainFromCmdParser $ do

  helpDesc <- peekCmdDesc

  addCmdSynopsis "a simple butcher example program"
  addCmdHelpStr "a very long help document"

  addCmd "version" $ do
    porcelain <- addSimpleBoolFlag "" ["porcelain"]
      (flagHelpStr "print nothing but the numeric version")
    addCmdHelpStr "prints the version of this program"
    addCmdImpl $ putStrLn $ if porcelain
      then "1.0"
      else "example, version 1.0"

  addCmd "help" $ addCmdImpl $ print $ ppHelpShallow helpDesc

  short <- addSimpleBoolFlag "" ["short"]
    (flagHelpStr "make the greeting short")
  name <- addParamString "NAME"
    (paramHelpStr "your name, so you can be greeted properly")

  addCmdImpl $ do
    if short
      then putStrLn $ "hi, " ++ name ++ "!"
      else putStrLn $ "hello, " ++ name ++ ", welcome from butcher!"
