{-# OPTIONS_GHC -Wall #-}
-- https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-All.html
-- to use QuickCheck.All
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

import Test.QuickCheck
import Test.QuickCheck.All
import Prelude hiding (mappend, mempty, mconcat)

-- Folds and Monoids
--
-- http://www.seas.upenn.edu/~cis194/spring13/lectures/07-folds-monoids.html
-- see cis194-07-suggestedReadingNotes.hs
-- =====================================================================
-- Folds Again
--
-- The take-away message is that we can implement a fold for many (though
-- not all) data types.
-- The fold for T will take one (higher-order) argument for each of T’s
--  constructors, encoding how to turn the values stored by that
-- constructor into a value of the result type—assuming that any
-- recursive occurrences of T have already been folded into a result.
-- Many functions we might want to write on T will end up being
--  expressible as simple folds.


data Tree a =   Empty
                | Node (Tree a) a (Tree a)
              deriving (Show, Eq)
leaf :: a -> Tree a
leaf x = Node Empty x Empty

-- write a function to compute the size of a tree
-- (i.e. the number of Nodes):
treeSize :: Num a => Tree a -> a
treeSize Empty    = 0
treeSize (Node lt _ rt) = 1 + treeSize lt + treeSize rt

-- flatten into a List
flatten :: Tree t -> [t]
flatten Empty = []
flatten (Node lt t rt) = flatten lt ++ [t] ++ flatten rt

-- Are you starting to see any patterns? Each of the above functions:
-- 1. takes a Tree as input
-- 2. pattern-matches on the input Tree
-- 3. in the Empty case, gives a simple answer
-- 4. in the Node case:
--     calls itself recursively on both subtrees
--     somehow combines the results from the recursive calls
--     with the data x to produce the final result
--
-- As good programmers, we always strive to abstract out repeating
-- patterns, right?
--
-- So let’s generalize. We’ll need to pass as parameters the parts of
-- the above examples which change from example to example:
-- 1. the return type (a)
-- 2. the answer in the Empty case  (e)
-- 3. how to combine the recursive calls (f)
-- e.g:
genericTreeFold :: a -> (a -> b -> a -> a) -> Tree b -> a
genericTreeFold e _ Empty = e
genericTreeFold e f (Node lt x rt) = f (genericTreeFold e f lt) x (genericTreeFold e f rt)

-- define treeSize, flatten etc more simply
treeSize' :: Tree a -> Integer
treeSize' = genericTreeFold 0 (\l _ r -> 1 + l + r)
treeSum' :: Tree Integer -> Integer
treeSum' = genericTreeFold 0 (\l av r -> av + l + r)
flatten' :: Tree a -> [a]
flatten' = genericTreeFold [] (\l av r -> l ++ [av] ++ r)
treeMax :: (Ord a, Bounded a) => Tree a -> a
treeMax = genericTreeFold minBound (\l av r -> l `max` av `max` r)

-- idea - to extend the folding idea from lists to other data types
-- Where else can we use Folds?
data ExprT = Lit Integer
           | Add ExprT ExprT
           | Mul ExprT ExprT
exprTFold :: (Integer -> t) -> (t -> t -> t) -> (t -> t -> t) -> ExprT -> t
exprTFold f _ _ (Lit i) = f i
exprTFold f addOp mulOp (Add l r) = addOp lhs rhs
                                where
                                  lhs = exprTFold f addOp mulOp l
                                  rhs = exprTFold f addOp mulOp r
exprTFold f addOp mulOp (Mul l r) = mulOp lhs rhs
                                where
                                  lhs = exprTFold f addOp mulOp l
                                  rhs = exprTFold f addOp mulOp r

-- expressing eval using this generic method...
eval :: ExprT -> Integer
eval = exprTFold id (+) (*)

-- Now we can easily do other things like count
-- the number of literals in an expression:
numLiterals :: ExprT -> Integer
numLiterals = exprTFold (const 1) (+) (+)

-- =====================================================================
-- Monoids
-- found in the Data.Monoid module
class Monoid m where
  mempty  :: m
  mappend :: m -> m -> m

  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
-- defined as a synonym to mappend
(<>) :: Main.Monoid m => m -> m -> m
(<>) = mappend
-- Types which are instances of Monoid have a special element
-- called mempty, and a binary operation mappend (abbreviated (<>))
-- which takes two values of the type and produces another one.
-- The intention is that mempty is an identity for <>,
--  and <> is associative; that is, for all x, y, and z,
-- 1. mempty <> x == x
-- 2. x <> mempty == x
-- 3. (x <> y) <> z == x <> (y <> z)
-- The associativity law means that we can unambiguously write things like
--  a <> b <> c <> d <> e
-- because we will get the same result no matter how we parenthesize.

-- Monoids show up everywhere, once you know to look for them
instance Main.Monoid [a] where
  mempty  = []
  mappend = (++)
-- addition defines a perfectly good monoid on integers
--  (or rational numbers, or real numbers…).
--  However, so does multiplication! What to do? We can’t
-- give two different instances of the same type class to the same type.
-- Instead, we create two newtypes, one for each instance:
newtype Sum a = Sum a
          deriving (Eq, Ord, Num, Show)
getSum :: Sum a -> a
getSum (Sum a) = a

instance Num a => Main.Monoid (Sum a) where
  mempty  = Sum 0
  mappend = (+)

lst :: [Integer]
lst = [1,5,8,23,423,99]
sumOfLst :: Integer
sumOfLst = getSum . mconcat . map Sum $ lst

-- Pairs form a monoid as long as the individual components do:
instance (Main.Monoid a, Main.Monoid b) => Main.Monoid (a,b) where
  mempty = (mempty, mempty)
  (a,b) `mappend` (c,d) = (a `mappend` c, b `mappend` d)


-- Challenge: can you make an instance of Monoid for Bool?
-- ----------
-- How many different instances are there?
--  one for And and one for Or, the two binary opersions on Bool
newtype All = All { getAll :: Bool }
      deriving (Eq, Ord, Show, Bounded)
instance Main.Monoid All where
  mempty = All True
  All x `mappend` All y = All (x && y)

newtype Any = Any {getAny :: Bool }
instance Main.Monoid Any where
  mempty = Any False
  Any x `mappend` Any y = Any (x || y)

-- Challenge: how would you make function types an instance of Monoid?
-- ----------
--

--
-- Wacky boilerplate to make all tests run.
return []
-- run this to test all quickcheck properties
runTests :: IO Bool
runTests = $quickCheckAll
