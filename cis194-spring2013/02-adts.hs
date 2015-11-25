-- https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-All.html
-- to use QuickCheck.All
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck()
import Test.QuickCheck.All

-- http://www.seas.upenn.edu/~cis194/spring13/lectures/02-ADTs.html
-- Suggested Reading - Real World Haskell, chapters 2 and 3
-- Excercises are in cis194-02-hw
-- tests using QuickCheck
-- Monadic testing using QuickCheck uses code from following site
-- https://www.fpcomplete.com/user/christianpbrink/quickcheck-and-webdriver

--
-- Enumeration types
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

-- function clauses are tried in order from top to bottom
-- we use this feature to avoid specifying every Thing
isSmall :: Thing -> Bool
isSmall Ship = False
isSmall King = False
isSmall _    = True


-- Beyond Enumerations - Algebraic Data types
-- In general, an algebraic data type has one or more data constructors,
--  and each data constructor can have zero or more arguments.
data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr4
          deriving Show
type Type11 = String
type Type12 = String
type Type21 = String
type Type31 = String
type Type32 = String
type Type33 = String
-- Can Be Recursive
data IntList = Empty
                | Cons Int IntList
data Tree = Leaf Char
          | Node Tree Int Tree
  deriving Show

tree :: Tree
tree = Node (Leaf 'x') 1 (Node (Leaf 'y') 2 (Leaf 'z'))

-- Pattern Matching
-- taking apart a value by finding out which constructor was used to build it
foo :: AlgDataType -> String
foo (Constr1 a b)   = "Constr1 was used with " ++ a ++ " and " ++ b
foo (Constr2 a)     = "Constr2 was used with " ++ a
foo x @ (Constr3 a b c) = "Constr3 was used with " ++ a ++ ", " ++ b ++ " and " ++ c  ++ "to build " ++ show x
foo Constr4         = "Constr4 was used to build"

-- Case Expressions
--
data FailableDouble = Failure
                      | OK Double

failureToZero :: FailableDouble -> Double
failureToZero x = case x of
  Failure -> 0
  OK d    -> d
-- Case expr above vs Pattern Matching below
safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x / y)

-- Wacky boilerplate to make all tests run.
return []
-- run this to test all quickcheck properties
runTests :: IO Bool
runTests = $quickCheckAll
