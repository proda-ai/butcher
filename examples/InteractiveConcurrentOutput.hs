module Main
  ( main
  )
where


import           Data.Char
import           Data.Functor.Identity

import           System.Console.Concurrent
import           System.Console.Regions
import           System.IO

import           UI.Butcher.Monadic


parser :: CmdParser Identity (IO ()) ()
parser = do
  addCmd "inner" $ do
    addCmdSynopsis "inner thingy"
    my <- addSimpleBoolFlag "" ["my"] mempty
    addCmdImpl $ do
      putStrLn $ "my = " ++ show my
      putStrLn "inner"
  addCmd "ither" $ do
    addCmdSynopsis "another inner command"
    addCmdImpl $ putStrLn "other"
  reorderStart
  foo      <- addSimpleBoolFlag "" ["foo"] (flagHelpStr "!foo help!")
  fooo     <- addSimpleBoolFlag "" ["fooo"] (flagHelpStr "!fooo help!")
  bar      <- addSimpleBoolFlag "" ["bar"] (flagHelpStr "!bar is useful!")
  x :: Int <- addFlagReadParam "x" [] "X" mempty
  _b       <- addSimpleBoolFlag "b" [] mempty
  reorderStop
  addCmdImpl $ do
    putStrLn $ "foo = " ++ show foo
    putStrLn $ "fooo = " ++ show fooo
    putStrLn $ "bar = " ++ show bar
    putStrLn $ "x = " ++ show x

main :: IO ()
main = displayConsoleRegions $ do
  withReg $ \reg1 -> withReg $ \reg2 -> withReg $ \reg3 -> do
    let Right desc = toCmdDesc Nothing parser
    let mainLoop s = do
          let info = runCmdParserFromDesc desc (InputString s) parser
          setConsoleRegion reg1 $ show (_ppi_interactiveHelp info 5)
          setConsoleRegion reg2 (s ++ _ppi_inputSugg info)
          setConsoleRegion reg3 s
          -- putStr s
          c <- getChar
          -- outputConcurrent (show (ord c) ++ "\n")
          -- print $ show $ ord 
          case ord c of
            127 -> do
              -- putStr [c]
              mainLoop (if null s then s else init s)
            10 -> do
              -- putStr (replicate 100 $ chr 127)
              mainLoop ""
            27 -> pure ()
            _  -> do
              -- putStr s -- [c]
              mainLoop (s ++ [c])
    hSetEcho stdin False
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    mainLoop ""
  where withReg = withConsoleRegion Linear
