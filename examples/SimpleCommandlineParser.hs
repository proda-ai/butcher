module Main where

import           UI.Butcher.Monadic



main :: IO ()
main = mainFromCmdParser $ do
  addCmd "inner" $ do
    my <- addSimpleBoolFlag "" ["my"] mempty
    addCmdImpl $ do
      putStrLn $ "my = " ++ show my
      putStrLn "inner"
  addCmd "other" $ do
    addCmdImpl $ putStrLn "other"
  reorderStart
  foo      <- addSimpleBoolFlag "" ["foo"] mempty
  bar      <- addSimpleBoolFlag "" ["bar"] mempty
  x :: Int <- addFlagReadParam "x" [] "X" mempty
  b        <- addSimpleBoolFlag "b" [] mempty
  reorderStop
  addCmdImpl $ do
    putStrLn $ "foo = " ++ show foo
    putStrLn $ "bar = " ++ show bar
    putStrLn $ "x = " ++ show x
    putStrLn $ "b = " ++ show b

