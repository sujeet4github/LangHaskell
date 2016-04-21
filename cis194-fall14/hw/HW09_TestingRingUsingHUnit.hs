module HW09_TestingRingUsingHUnit where

-- http://www.seas.upenn.edu/~cis194/fall14/hw/09-testing.pdf

import Test.HUnit
import Ring

-- Ex 9
parserTests :: Test
parserTests = TestList [
    "Simple Test from Test Description" ~:
        parseAll "3" ~?= Just (3 :: Integer)

    ,
    "parse a 2x2 matrix" ~:
        parseAll "[[1,2][3,4]]" ~?= Just (MkMat 1 2 3 4)

    ,
    "parse a MkMod" ~:
        parseAll "3" ~?= Just (MkMod 3)

    ,
    "parse another MkMod" ~:
        parseAll "5" ~?= Just (MkMod 0)

    ,
    parseAll "True" ~?= Just True,
    parseAll "False" ~?= Just False

    ]
