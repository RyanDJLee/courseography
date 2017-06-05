{-|
Description: Test Course Requirement Parsers using HUnit Testing Framework.

Module containing test cases for Requirement Parsers.

-}

module ParserTests.ParserTests
(  reqTestSuite  ) where

import qualified Text.Parsec as Parsec
import Text.Parsec.String (Parser)
import Database.Requirement
import WebParsing.ReqParser
import Test.HUnit ( assertEqual, Test(..) )

-- Function to facilitate test case creation given a string, Req tuple
createTest :: (Eq a, Show a) => Parser a -> (String, a) -> Test
createTest parser (input, expected) = let courseReq =  Parsec.parse parser "" input
                                      in TestCase $ (assertEqual ("for (" ++ input ++ "),") (Right expected) courseReq)

-- categoryParser Tests
orInputs :: [(String, Req)]
orInputs = [("CSC120H1/CSC148H1", OR [J "CSC120H1", J "CSC148H1"])
           , ("CSC108H1/CSC120H1/CSC148H1", OR [J "CSC108H1", J "CSC120H1", J "CSC148H1"])]
andInputs :: [(String, Req)]
andInputs = [("CSC165H1, CSC236H1", AND [J "CSC165H1", J "CSC236H1"])
           , ("CSC120H1, CSC121H1, CSC148H1", AND [J "CSC120H1", J "CSC121H1", J "CSC148H1"])]
andorInputs :: [(String, Req)]
andorInputs = [("CSC148H1/CSC207H1, CSC165H1/CSC236H1", AND [OR [J "CSC148H1", J "CSC207H1"], OR [J "CSC165H1", J "CSC236H1"]])]

parInputs :: [(String, Req)]
parInputs = [("(CSC148H1)", J "CSC148H1")
           , ("CSC108H1, (CSC165H1/CSC148H1)", AND [J "CSC108H1", OR [J "CSC165H1", J "CSC148H1"]])
           , ("(MAT135H1, MAT136H1)/ MAT137Y1", OR [AND [J "MAT135H1", J "MAT136H1"], J "MAT137Y1"])
           , ("CSC148H1/(CSC108H1/CSC120H1, MAT137Y1/MAT157Y1)", OR [J "CSC148H1", AND [OR [J "CSC108H1", J "CSC120H1"], OR [J "MAT137Y1", J "MAT157Y1"]]])
           , ("STA247H1/STA255H1/STA257H1/PSY201H1/ECO227Y1, (MAT135H1, MAT136H1)/MAT137Y1/MAT157Y1", AND [OR [J "STA247H1", J "STA255H1", J "STA257H1", J "PSY201H1", J "ECO227Y1"], OR [AND [J "MAT135H1", J "MAT136H1"], J "MAT137Y1", J "MAT157Y1"]])]


fromInputs:: [(String, Req)]
fromInputs = [("1.0 FCE from the following: CSC148H1", FROM "1.0" $ J "CSC148H1")
           , ("2.0 FCEs from CSC108H1, CSC165H1/CSC148H1", FROM "2.0" $ AND [J "CSC108H1", OR [J "CSC165H1", J "CSC148H1"]])
           , ("2 fces from: MAT135H1, MAT136H1/ MAT137Y1", FROM "2" $ AND [J "MAT135H1",OR [J "MAT136H1",J "MAT137Y1"]])]


orTests :: Test
orTests = TestLabel "Basic or Requirement"  $ TestList $ (map (createTest categoryParser) orInputs)
andTests :: Test
andTests = TestLabel "Basic and Requirement" $ TestList $ (map (createTest categoryParser) andInputs)
andorTests :: Test
andorTests = TestLabel "Basic and-or-mixed Requirement" $ TestList $ (map (createTest categoryParser) andorInputs)
parTests :: Test
parTests = TestLabel "Basic and-or-paranthesized Requirement" $ TestList $ (map (createTest categoryParser) parInputs)
fromTests :: Test
fromTests = TestLabel "Basic From Requirements with integer or float fces" $ TestList $ (map (createTest categoryParser) fromInputs)

-- functions for running tests in REPL
reqTestSuite :: Test
reqTestSuite = TestLabel "ReqParser tests" $ TestList [fromTests]
