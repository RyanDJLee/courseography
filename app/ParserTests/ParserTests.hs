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
reqInputs1 :: [(String, Req)]
reqInputs1 = [("CSC120H1/CSC148H1", OR [J "CSC120H1", J "CSC148H1"])]
reqInputs2 :: [(String, Req)]
reqInputs2 = [("CSC120H1, CSC121H1, CSC148H1", AND [J "CSC120H1", J "CSC121H1", J "CSC148H1"])]
reqInputs3 :: [(String, Req)]
reqInputs3 = [("CSC148H1/(CSC108H1/CSC120H1, MAT137Y1/MAT157Y1)", OR [J "CSC148H1", AND [OR [J "CSC108H1", J "CSC120H1"], OR [J "MAT137Y1", J "MAT157Y1"]]])]
reqInputs4 :: [(String, Req)]
reqInputs4 = [("STA247H1/STA255H1/STA257H1/PSY201H1/ECO227Y1, (MAT135H1, MAT136H1)/MAT137Y1/MAT157Y1", AND [OR [J "STA247H1", J "STA255H1", J "STA257H1", J "PSY201H1", J "ECO227Y1"], OR [AND [J "MAT135H1", J "MAT136H1"], J "MAT137Y1", J "MAT157Y1"]])]

reqTests1 :: Test
reqTests1 = TestLabel "basic categoryParser" $ TestList $ (map (createTest categoryParser) reqInputs1)
reqTests2 :: Test
reqTests2 = TestLabel "basic categoryParser" $ TestList $ (map (createTest categoryParser) reqInputs2)
reqTests3 :: Test
reqTests3 = TestLabel "basic categoryParser" $ TestList $ (map (createTest categoryParser) reqInputs3)
reqTests4 :: Test
reqTests4 = TestLabel "basic categoryParser" $ TestList $ (map (createTest categoryParser) reqInputs4)

-- functions for running tests in REPL
reqTestSuite :: Test
reqTestSuite = TestLabel "Req parser tests" $ TestList [reqTests1, reqTests2, reqTests3, reqTests4]
