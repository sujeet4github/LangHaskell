{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# OPTIONS_GHC -Wall #-}

module Calc where

import Test.QuickCheck
import Test.QuickCheck.All()
import Parser
import StackVM as StackVM
import qualified Data.Map as M

class Expr a where
  lit :: Integer -> a
  add :: a -> a -> a
  mul :: a -> a -> a

-- Note that in order to make an instance for Program (which is a
-- type synonym) you will need to enable the TypeSynonymInstances
-- language extension, which you can do by adding
-- {-# LANGUAGE TypeSynonymInstances #-}
-- as the first line in your file.
instance Expr Program where
  lit a = [StackVM.PushI a]
  add l r = l ++ r ++ [StackVM.Add]
  mul l r = l ++ r ++ [StackVM.Mul]

compile :: String -> Maybe Program
compile str = parseExp lit add mul str :: Maybe Program

-- Exercise 6
--
-- Some users of your calculator have requested the ability to give
-- names to intermediate values and then reuse these stored values
-- later. To enable this, you first need to give arithmetic expressions the
-- ability to contain variables. Create a new type class HasVars a which
-- contains a single method var
--
-- Thus, types which are instances of HasVars have some notion of named variables.
class HasVars a where
  var :: String -> a

--
-- new data type VarExprT which is the same as ExprT but with an extra constructor for variables.
data VarExprT = Lit Integer
           | Var String
           | Add VarExprT VarExprT
           | Mul VarExprT VarExprT
  deriving (Show, Eq)

-- Make VarExprT an instance of both Expr and HasVars.
instance Expr VarExprT where
  lit = Calc.Lit
  add = Calc.Add
  mul = Calc.Mul
instance HasVars VarExprT where
  var = Var
-- You should now be able to write things like  *Calc> add (lit 3) (var "x") :: VarExprT
test_VarExprT :: Bool
test_VarExprT = lhs == rhs where
                            lhs = Calc.Add (Lit 3) (Var "x")
                            rhs = add (lit 3) (var "x") :: VarExprT
test_VarExprT2 :: Bool
test_VarExprT2 = let        lhs = Calc.Add (Lit 3) (Var "x")
                            rhs = add (lit 3) (var "x") :: VarExprT
                in lhs == rhs

-- Note: to write these instances you will need to enable the FlexibleInstances

-- variables can be interpreted as functions from a mapping of variables
-- to Integer values to (possibly) Integer values.
-- It should work by looking up the variable in the mapping.
instance HasVars (M.Map String Integer -> Maybe Integer) where
  var v = \mapEnv -> M.lookup v mapEnv
-- A very advanced Soln
--  var = M.lookup

-- How to think about this?
-- https://www.reddit.com/r/haskellquestions/comments/33darh/cis_194_homework_5_question_6_problems/
--
--  So - you want to write a typeclass for
--  a function type - (M.Map String Integer -> Maybe Integer)
--
-- The argument to the function represents a thing called environment.
-- The function takes the environment and computes something according to the environment.
--
-- Suppose you have a symbolic calculator. You feed it a symbolic expression,
-- say "2*x + 3*y^2 -1", and an environment that says [(x,5), (y,-1), (z,3)],
-- and it parses the expression and substitutes the variables and computes
-- Just $ 2*5 + 3*(-1)^2 - 1.
-- Or you feed it the same expression and [(x,5),(z,3)] as the environment,
-- and it computes Nothing, because it needs y and there's no y in the environment.
-- if you feed the calculator only a symbolic expression, it will return a
-- function of the exact type you need to write an instance for.

-- these same functions can be interpreted as expressions (by passing along
-- the mapping to subexpressions and combining results appropriately).
instance Expr (M.Map String Integer -> Maybe Integer) where
  lit v = \_ -> Just v
  add l r = \env -> do
                    lv <- l env
                    rv <- r env
                    return (lv + rv)
  mul l r = \env -> do
                    lv <- l env
                    rv <- r env
                    return (lv * rv)
{- A very advanced Soln
  lit val _ = Just val
  add v1 v2 m = do
                  i1 <- v1 m
                  i2 <- v2 m
                  return (i1 + i2)
  mul v1 v2 m = do
                  i1 <- v1 m
                  i2 <- v2 m
                  return (i1 * i2)
-}

-- Once you have created these instances, you should be able to test them as follows
--
withVars :: [(String, Integer)]
            -> (M.Map String Integer -> Maybe Integer)
            -> Maybe Integer
withVars vs ex = ex $ M.fromList vs

main :: IO ()
main = do
          print $ withVars [("x", 6)] $ add (lit 3) (var "x")
          print $ withVars [("x", 6)] $ add (lit 3) (var "y")
          print $ withVars  [("x", 6), ("y", 3)] $ mul (var "x") (add (var "y") (var "x"))

-------------------------------------------------------------------------------
-- Wacky boilerplate to make all tests run.
return []
-- run this to test all quickcheck properties
runTests :: IO Bool
runTests = $quickCheckAll
