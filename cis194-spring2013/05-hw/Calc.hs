{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import Test.QuickCheck
import Test.QuickCheck.All()
import ExprT
import Parser


-- Exercise 1
-- Write Version 1 of the calculator: an evaluator for ExprT, with the
-- signature
eval :: ExprT -> Integer
eval (Lit n) = n
eval (Mul l r) = eval l * eval r
eval (Add l r) = eval l + eval r

prop_eval_eg1 :: Bool
prop_eval_eg1 = eval (Mul (Add (Lit 2) (Lit 3)) (Lit 4)) == 20


-- Exercise 2
--
evalStr :: String -> Maybe Integer
evalStr cs = let parsedExpr = parseExp Lit Add Mul cs
              in case parsedExpr of
                Nothing       -> Nothing
                Just exprStr  -> Just (eval exprStr)
{-
evalStr cs = do
                parsedExpr <- parseExp Lit Add Mul cs
                return (eval parsedExpr)
-}

-- Exercise 3
--
{-
-- My Earlier misguided attempt

class Expr t where
  lit :: t -> ExprT
  add :: t -> t -> ExprT
  mul :: t -> t -> ExprT

instance Expr Integer where
    lit n = Lit n
    add l r = Add (lit l) (lit r)
    mul l r = Mul (lit l) (lit r)
instance Expr ExprT where
    lit e = e
    add l r = Add l r
    mul l r = Mul l r
-}
class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

instance Expr ExprT where
  lit = Lit
  add = Add
  mul = Mul

-- :t mul (add (lit 2) (lit 3)) (lit 4)
-- returns
-- mul (add (lit 2) (lit 3)) (lit 4) :: Expr a => a
--
-- What does this mean?
--  The expression mul (add (lit 2) (lit 3)) (lit 4)
-- has any type which is an instance of the Expr type class
-- So writing it by itself is ambiguous
-- you get an error on ghci from show
-- "No instance for (Show a0) arising from a use of `print'
--    The type variable `a0' is ambiguous"
--
-- One way to resolve the ambiguity is by giving an explicit type
-- signature.
prop_expr_with_explicit_type_signature :: Bool
prop_expr_with_explicit_type_signature = actual == expected
          where
              expected = Mul (Add (Lit 2) (Lit 3)) (Lit 4)
              actual = mul (add (lit 2) (lit 3)) (lit 4) :: ExprT
-- another way is using such an expression as part of some larger
-- expression so that the context in which it is used determines
-- the type.
-- e.g.
reify :: ExprT -> ExprT
reify = id
-- To the untrained eye it may look like reify does no actual work!
-- But its real purpose is to constrain the type of its argument to ExprT.
-- Now we can write things like
-- reify $ mul (add (lit 2) (lit 3)) (lit 4)
-- at the ghci prompt


-- Exercise 4
--
instance Expr Integer where
  lit = id
  add = (+)
  mul = (*)

instance Expr Bool where
  lit = (> 0)
  add = (||)
  mul = (&&)

newtype MinMax = MinMax Integer deriving (Eq, Show)
instance Expr MinMax where
  lit = MinMax
  -- 1st try: - problem with this is l anr r are treated as MinMax
  -- and not Integer, hence it looks like we are comparing MinMax
  -- which needs Ord
  -- add l r = if (l > r) then l else r
  -- mul l r = if (l < r) then l else r
  -- 2nd try: min/max operators too require Ord
  -- add = max
  -- mul = min
  (MinMax l) `add` (MinMax r) = MinMax (if l > r then l else r)
  (MinMax l) `mul` (MinMax r) = MinMax (if l < r then l else r)


newtype Mod7 = Mod7 Integer deriving (Eq, Show)
instance Expr Mod7 where
  lit = Mod7
  add (Mod7 l) (Mod7 r) = Mod7 ((l + r) `mod` 7)
  mul (Mod7 l) (Mod7 r) = Mod7 ((l * r) `mod` 7)

testExp :: Expr a => Maybe a
testExp = parseExp lit add mul "(3 * -4) + 5"

testInteger :: Maybe Integer
testInteger = testExp :: Maybe Integer
testBool :: Maybe Bool
testBool = testExp :: Maybe Bool
testMM :: Maybe MinMax
testMM = testExp :: Maybe MinMax
testSat :: Maybe Mod7
testSat = testExp :: Maybe Mod7

-------------------------------------------------------------------------------
-- Wacky boilerplate to make all tests run.
return []
-- run this to test all quickcheck properties
runTests :: IO Bool
runTests = $quickCheckAll
