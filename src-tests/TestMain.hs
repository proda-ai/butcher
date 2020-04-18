module Main where



#include "prelude.inc"

import           Test.Hspec

-- import NeatInterpolation

import           UI.Butcher.Monadic
import           UI.Butcher.Monadic.Types



main :: IO ()
main = hspec $ tests

tests :: Spec
tests = do
  describe "checkTests"      checkTests
  describe "simpleParseTest" simpleParseTest
  describe "simpleRunTest"   simpleRunTest


checkTests :: Spec
checkTests = do
  before_ pending $ it "check001" $ True `shouldBe` True


simpleParseTest :: Spec
simpleParseTest = do
  it "failed parse 001"
    $ let r = runCmdParserSimpleString "foo" testCmd1
      in  r `shouldSatisfy` Data.Either.isLeft
  it "toplevel" $ (testParse testCmd1 "") `shouldBe` Nothing
  it "hasImpl 001" $ (testParse testCmd1 "abc") `shouldSatisfy` Maybe.isJust
  it "hasImpl 002" $ (testParse testCmd1 "def") `shouldSatisfy` Maybe.isJust


simpleRunTest :: Spec
simpleRunTest = do
  it "failed run" $ testRun testCmd1 "" `shouldBeRight` Nothing
  describe "no reordering" $ do
    it "cmd 1" $ testRun testCmd1 "abc" `shouldBeRight` (Just 100)
    it "cmd 2" $ testRun testCmd1 "def" `shouldBeRight` (Just 200)
    it "flag 1" $ testRun testCmd1 "abc -f" `shouldBeRight` (Just 101)
    it "flag 2" $ testRun testCmd1 "abc --flong" `shouldBeRight` (Just 101)
    it "flag 3" $ testRun testCmd1 "abc -f -f" `shouldBeRight` (Just 101)
    it "flag 4" $ testRun testCmd1 "abc -f -g" `shouldBeRight` (Just 103)
    it "flag 5"
      $               testRun testCmd1 "abc -f -g -f"
      `shouldSatisfy` Data.Either.isLeft -- no reordering
    it "flag 6"
      $               testRun testCmd1 "abc -g -f"
      `shouldSatisfy` Data.Either.isLeft -- no reordering
    it "flag 7" $ testRun testCmd1 "abc -g -g" `shouldBeRight` (Just 102)
  describe "with reordering" $ do
    it "cmd 1" $ testRun testCmd2 "abc" `shouldBeRight` (Just 100)
    it "cmd 2" $ testRun testCmd2 "def" `shouldBeRight` (Just 200)
    it "flag 1" $ testRun testCmd2 "abc -f" `shouldBeRight` (Just 101)
    it "flag 2" $ testRun testCmd2 "abc --flong" `shouldBeRight` (Just 101)
    it "flag 3" $ testRun testCmd2 "abc -f -f" `shouldBeRight` (Just 101)
    it "flag 4" $ testRun testCmd2 "abc -f -g" `shouldBeRight` (Just 103)
    it "flag 5" $ testRun testCmd2 "abc -f -g -f" `shouldBeRight` (Just 103)
    it "flag 6" $ testRun testCmd2 "abc -g -f" `shouldBeRight` (Just 103)
    it "flag 7" $ testRun testCmd2 "abc -g -g" `shouldBeRight` (Just 102)
  describe "with action" $ do
    it "flag 1" $ testRunA testCmd3 "abc" `shouldBeRight` 0
    it "flag 2" $ testRunA testCmd3 "abc -f" `shouldBeRight` 1
    it "flag 3" $ testRunA testCmd3 "abc -g" `shouldBeRight` 2
    it "flag 4" $ testRunA testCmd3 "abc -f -g" `shouldBeRight` 3
    it "flag 5" $ testRunA testCmd3 "abc -g -f" `shouldBeRight` 3
  describe "read flags" $ do
    it "flag 1" $ testRun testCmd5 "abc" `shouldBeRight` (Just 10)
    it "flag 2" $ testRun testCmd5 "abc -f 2" `shouldBeRight` (Just 2)
    it "flag 3" $ testRun testCmd5 "abc --flag 3" `shouldBeRight` (Just 3)
    it "flag 4" $ testRun testCmd5 "abc -f=4" `shouldBeRight` (Just 4)
    it "flag 5" $ testRun testCmd5 "abc --flag=5" `shouldBeRight` (Just 5)
    it "flag 6" $ testRun testCmd5 "abc -f" `shouldSatisfy` Data.Either.isLeft
    it "flag 7"
      $               testRun testCmd5 "abc -flag 0"
      `shouldSatisfy` Data.Either.isLeft
    it "flag 8"
      $               testRun testCmd5 "abc --f 0"
      `shouldSatisfy` Data.Either.isLeft
  describe "addParamStrings" $ do
    it "case 1" $ testRun' testCmd6 "" `shouldBeRight` (Just ([], 0))
    it "case 2" $ testRun' testCmd6 "-f" `shouldBeRight` (Just ([], 1))
    it "case 3" $ testRun' testCmd6 "abc" `shouldBeRight` (Just (["abc"], 0))
    it "case 4"
      $               testRun' testCmd6 "abc def"
      `shouldBeRight` (Just (["abc", "def"], 0))
    it "case 5"
      $               testRun' testCmd6 "-g abc def"
      `shouldBeRight` (Just (["abc", "def"], 2))
    it "case 6"
      $               testRun' testCmd6 "-f -g def"
      `shouldBeRight` (Just (["def"], 3))
  describe "addParamNoFlagStrings" $ do
    it "case 1" $ testRun' testCmd7 "" `shouldBeRight` (Just ([], 0))
    it "case 2" $ testRun' testCmd7 "-f" `shouldBeRight` (Just ([], 1))
    it "case 3" $ testRun' testCmd7 "abc" `shouldBeRight` (Just (["abc"], 0))
    it "case 4" $ testRun' testCmd7 "abc -f" `shouldBeRight` (Just (["abc"], 1))
    it "case 5"
      $               testRun' testCmd7 "-g abc -f"
      `shouldBeRight` (Just (["abc"], 3))
    it "case 6"
      $               testRun' testCmd7 "abc -g def"
      `shouldBeRight` (Just (["abc", "def"], 2))
  describe "defaultParam" $ do
    it "case  1" $ testRun testCmdParam "" `shouldSatisfy` Data.Either.isLeft
    it "case  2" $ testRun testCmdParam "n" `shouldSatisfy` Data.Either.isLeft
    it "case  3" $ testRun testCmdParam "y" `shouldSatisfy` Data.Either.isLeft
    it "case  4" $ testRun testCmdParam "False n" `shouldBeRight` (Just 110)
    it "case  5" $ testRun testCmdParam "False y" `shouldBeRight` (Just 310)
    it "case  6" $ testRun testCmdParam "True n" `shouldBeRight` (Just 1110)
    it "case  7" $ testRun testCmdParam "True y" `shouldBeRight` (Just 1310)
    it "case  8" $ testRun testCmdParam "1 False y" `shouldBeRight` (Just 301)
    it "case  9"
      $               testRun testCmdParam "1 False y def"
      `shouldBeRight` (Just 201)
    it "case 10"
      $               testRun testCmdParam "1 False 2 y def"
      `shouldBeRight` (Just 203)
    it "case 11"
      $               testRun testCmdParam "1 True 2 y def"
      `shouldBeRight` (Just 1203)
  describe "completions" $ do
    it "case  1" $ testCompletion completionTestCmd "" `shouldBe` ""
    it "case  2" $ testCompletion completionTestCmd "a" `shouldBe` "bc"
    it "case  3" $ testCompletion completionTestCmd "abc" `shouldBe` ""
    it "case  4" $ testCompletion completionTestCmd "abc " `shouldBe` "-"
    it "case  5" $ testCompletion completionTestCmd "abc -" `shouldBe` ""
    it "case  6" $ testCompletion completionTestCmd "abc --" `shouldBe` "flag"
    it "case  7" $ testCompletion completionTestCmd "abc -f" `shouldBe` ""
    it "case  8" $ testCompletion completionTestCmd "abcd" `shouldBe` "ef"
    it "case  9" $ testCompletion completionTestCmd "gh" `shouldBe` "i"
    it "case 10" $ testCompletion completionTestCmd "ghi" `shouldBe` ""
    it "case 11" $ testCompletion completionTestCmd "ghi " `shouldBe` "jkl"



testCmd1 :: CmdParser Identity (WriterS.Writer (Sum Int) ()) ()
testCmd1 = do
  addCmd "abc" $ do
    f <- addSimpleBoolFlag "f" ["flong"] mempty
    g <- addSimpleBoolFlag "g" ["glong"] mempty
    addCmdImpl $ do
      when f $ WriterS.tell 1
      when g $ WriterS.tell 2
      WriterS.tell 100
  addCmd "def" $ do
    addCmdImpl $ do
      WriterS.tell 200

testCmd2 :: CmdParser Identity (WriterS.Writer (Sum Int) ()) ()
testCmd2 = do
  addCmd "abc" $ do
    reorderStart
    f <- addSimpleBoolFlag "f" ["flong"] mempty
    g <- addSimpleBoolFlag "g" ["glong"] mempty
    reorderStop
    addCmdImpl $ do
      when f $ WriterS.tell 1
      when g $ WriterS.tell 2
      WriterS.tell 100
  addCmd "def" $ do
    addCmdImpl $ do
      WriterS.tell 200

testCmd3 :: CmdParser (StateS.State Int) () ()
testCmd3 = do
  addCmd "abc" $ do
    reorderStart
    addSimpleFlagA "f" ["flong"] mempty (StateS.modify (+ 1))
    addSimpleFlagA "g" ["glong"] mempty (StateS.modify (+ 2))
    reorderStop
    addCmdImpl ()
  addCmd "def" $ do
    addCmdImpl ()

testCmd4 :: CmdParser Identity (WriterS.Writer (Sum Int) ()) ()
testCmd4 = do
  addCmd "a" $ do
    addCmd "aa" $ do
      addCmdImpl $ WriterS.tell 1
  addCmd "b" $ do
    addCmd "bb" $ do
      addCmdImpl $ WriterS.tell 4
  addCmd "a" $ do
    addCmd "ab" $ do
      addCmdImpl $ WriterS.tell 2
  addCmd "b" $ do
    addCmd "ba" $ do
      addCmdImpl $ WriterS.tell 3

testCmd5 :: CmdParser Identity (WriterS.Writer (Sum Int) ()) ()
testCmd5 = do
  addCmd "abc" $ do
    x <- addFlagReadParam "f" ["flag"] "flag" (flagDefault (10 :: Int))
    addCmdImpl $ WriterS.tell (Sum x)

testCmd6 :: CmdParser Identity (WriterS.Writer (Sum Int) [String]) ()
testCmd6 = do
  f    <- addSimpleBoolFlag "f" ["flong"] mempty
  g    <- addSimpleBoolFlag "g" ["glong"] mempty
  args <- addParamStrings "ARGS" mempty
  addCmdImpl $ do
    when f $ WriterS.tell 1
    when g $ WriterS.tell 2
    pure args

testCmd7 :: CmdParser Identity (WriterS.Writer (Sum Int) [String]) ()
testCmd7 = do
  reorderStart
  f    <- addSimpleBoolFlag "f" ["flong"] mempty
  g    <- addSimpleBoolFlag "g" ["glong"] mempty
  args <- addParamNoFlagStrings "ARGS" mempty
  reorderStop
  addCmdImpl $ do
    when f $ WriterS.tell 1
    when g $ WriterS.tell 2
    pure args

testCmdParam :: CmdParser Identity (WriterS.Writer (Sum Int) ()) ()
testCmdParam = do
  p :: Int <- addParamRead "INT" (paramDefault 10)
  b        <- addParamRead "MANDR" mempty
  r        <- addParamReadOpt "MAY1" (paramDefault 20)
  s        <- addParamString "MAND" mempty
  q        <- addParamString "STR" (paramDefault "abc")
  addCmdImpl $ do
    WriterS.tell (Sum p)
    when (q == "abc") $ WriterS.tell 100
    r `forM_` (WriterS.tell . Sum)
    when b $ WriterS.tell $ Sum 1000
    when (s == "y") $ WriterS.tell 200
    pure ()

completionTestCmd :: CmdParser Identity () ()
completionTestCmd = do
  addCmd "abc" $ do
    _ <- addSimpleBoolFlag "f" ["flag"] mempty
    addCmdImpl ()
  addCmd "abcdef" $ do
    _ <- addSimpleBoolFlag "f" ["flag"] mempty
    addCmdImpl ()
  addCmd "ghi" $ do
    addCmd "jkl" $ do
      addCmdImpl ()

testCompletion :: CmdParser Identity a () -> String -> String
testCompletion p inp =
  _ppi_inputSugg $ runCmdParser Nothing (InputString inp) p


testParse :: CmdParser Identity out () -> String -> Maybe out
testParse cmd s = case runCmdParserSimpleString s cmd of
  Left{}  -> Nothing
  Right o -> Just o

testRun
  :: CmdParser Identity (WriterS.Writer (Sum Int) ()) ()
  -> String
  -> Either ParsingError (Maybe Int)
testRun cmd s =
  fmap (fmap (getSum . WriterS.execWriter)) $ _ppi_value $ runCmdParser
    Nothing
    (InputString s)
    cmd

testRun'
  :: CmdParser Identity (WriterS.Writer (Sum Int) a) ()
  -> String
  -> Either ParsingError (Maybe (a, Int))
testRun' cmd s =
  fmap (fmap (fmap getSum . WriterS.runWriter)) $ _ppi_value $ runCmdParser
    Nothing
    (InputString s)
    cmd

testRunA
  :: CmdParser (StateS.State Int) () () -> String -> Either ParsingError Int
testRunA cmd str = case StateS.runState act (0 :: Int) of
  (info, s) -> _ppi_value info $> s
  where act = runCmdParserA Nothing (InputString str) cmd

getDoc :: String -> CmdParser Identity out () -> CommandDesc
getDoc s p = _ppi_mainDesc $ runCmdParser (Just "test") (InputString s) p


shouldBeRight :: (Show l, Show r, Eq r) => Either l r -> r -> Expectation
shouldBeRight x y = x `shouldSatisfy` \case
  Left{}  -> False
  Right r -> r == y
