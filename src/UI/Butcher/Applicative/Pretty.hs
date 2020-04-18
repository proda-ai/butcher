
-- | Pretty-print of CommandDescs. To explain what the different functions
-- do, we will use an example CmdParser. The CommandDesc derived from that
-- CmdParser will serve as example input to the functions in this module.
--
-- > main = mainFromCmdParserWithHelpDesc $ \helpDesc -> do
-- > 
-- >   addCmdSynopsis "a simple butcher example program"
-- >   addCmdHelpStr "a very long help document"
-- > 
-- >   addCmd "version" $ do
-- >     porcelain <- addSimpleBoolFlag "" ["porcelain"]
-- >       (flagHelpStr "print nothing but the numeric version")
-- >     addCmdHelpStr "prints the version of this program"
-- >     addCmdImpl $ putStrLn $ if porcelain
-- >       then "0.0.0.999"
-- >       else "example, version 0.0.0.999"
-- > 
-- >   addCmd "help" $ addCmdImpl $ print $ ppHelpShallow helpDesc
-- > 
-- >   short <- addSimpleBoolFlag "" ["short"] (flagHelpStr "make the greeting short")
-- >   name <- addStringParam "NAME"
-- >     (paramHelpStr "your name, so you can be greeted properly")
-- > 
-- >   addCmdImpl $ do
-- >     if short
-- >       then putStrLn $ "hi, " ++ name ++ "!"
-- >       else putStrLn $ "hello, " ++ name ++ ", welcome from butcher!"
module UI.Butcher.Applicative.Pretty
  ( ppUsage
  , ppUsageShortSub
  , ppUsageAt
  , ppHelpShallow
  , ppHelpDepthOne
  , ppUsageWithHelp
  , ppPartDescUsage
  , ppPartDescHeader
  , parsingErrorString
  , descendDescTo
  )
where



import           UI.Butcher.Internal.Pretty
