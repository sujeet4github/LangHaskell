{-
Exercise 4 Boolean arithmetic forms a ring. Boolean-and (conjunction)
is the multiplication operation, but Boolean-or is not the addition
operation. What is? (There aren’t too many choices here!) Write
Ring and Parsable instances for Bool.

Test your instances!
-}

module HW05_Ex4 where

import HW05_Ring
import HW05_Parser
import Data.Maybe    ( listToMaybe )
import Test.HUnit

instance Ring Bool where
  addId  = False
  addInv = not
  mulId  = True

  add = (||)
  mul = (&&)

instance Parsable Bool where
  parse = listToMaybe . reads

-- To test:
-- at ghci prompt:
--  runTestTT testBool
--
testBool :: Test
testBool = TestList [
  "test boolean True" ~: (Just (True, "") :: Maybe (Bool, String))  ~=? (parse "True"),
  "test boolean True" ~: (Just (False, " we are") :: Maybe (Bool, String))  ~=? (parse "False we are"),
  "addId"  ~: False ~=? addId,
  "negate" ~: False ~=? addInv True,
  "mulId"  ~: True ~=?  mulId,
  "add"    ~: True ~=? add True False,
  "mul"    ~: False ~=? mul True False
  ]