{-# OPTIONS_GHC -Wall #-}
-- https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-All.html
-- to use QuickCheck.All
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All

import Data.Monoid

-- Suggested Reading:
-- Learn You a Haskell, Only folds and horses
--   http://learnyouahaskell.com/higher-order-functions#folds
--   see week 4 lecture notes - for notes from this site
-- ==========================================================
-- Learn You a Haskell, Monoids
--   http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids
-- Monoids
-- -------
-- * is a type class - defined in import Data.Monoid
-- * is when you have an associative binary function and a value which
--   acts as an identity with respect to that function.
--
-- Only concrete types can be made instances of Monoid
-- As it is defined
-- ----------------
-- class Monoid m where
--    -- mempty: represents the identity value
--    mempty :: m
--    -- mappend: the binary function, unfortunate choice of name :-)
--    mappend :: m -> m -> m
--    -- mconcat: has a default implementation, which just takes mempty as a
--    --  starting value and folds the list from the right with mappend.
--    -- Because the default implementation is fine for most instances,
--    -- for some instances, there may be a more efficient implementation
--    mconcat :: [m] -> m
--    mconcat = foldr mappend mempty
--
-- The Monoid Laws
-- ---------------
-- mempty `mappend` x == x
-- x `mappend` mempty == x
-- (x `mappend` y) `mappend` z == x `mappend` (y `mappend` z)
--
-- The module Data.Monoid defines the infix operator (<>) as a synonym for mappend

-- Haskell does not enforce these laws, it is up to the programmer
-- to make sure that these are obeyed by instances
-- Two Ways:
-- 1.
prop_list_leftIdentity :: Eq t => [t] -> Bool
prop_list_leftIdentity xs = [] `mappend` xs == xs
prop_list_rightIdentity :: Eq t => [t] -> Bool
prop_list_rightIdentity xs = xs `mappend` [] == xs
prop_listInt_associativity :: [Int] -> [Int] -> [Int] -> Bool
prop_listInt_associativity x y z = ((x <> y) <> z) == (x <> (y <> z))
prop_listChar_associativity :: [Char] -> [Char] -> [Char] -> Bool
prop_listChar_associativity x y z = ((x <> y) <> z) == (x <> (y <> z))

-- 2.
monoidRightIdProp :: (Eq m, Monoid m) => m -> Bool
monoidRightIdProp x = x == (x <> mempty)
monoidLeftIdProp :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdProp x = x == (mempty <> x)
monoidAssocProp :: (Eq m, Monoid m) => m -> m -> m -> Bool
monoidAssocProp x y z = (x <> (y <> z)) == ((x <> y) <> z)
t1 :: IO ()
t1 = quickCheck (monoidRightIdProp :: [Int] -> Bool)
t2 :: IO ()
t2 = quickCheck (monoidLeftIdProp :: [Int] -> Bool)
t3 :: IO ()
t3 = quickCheck (monoidAssocProp :: [Int] -> [Int] -> [Int] -> Bool)

-- Some Monoids defined in Data.Monoid
-- Product  - * on Num constrained classes
--  getProduct $ Product 3 `mappend` Product 9
-- Sum - + on Num constrained classes
--  getSum $ Sum 2 `mappend` Sum 9
--  getSum . mconcat . map Sum $ [1,2,3]
--
-- ==========================================================
-- Fold from the Haskell wiki
--   http://haskell.org/haskellwiki/Fold
-- ==========================================================
-- Heinrich Apfelmus, Monoids and Finger Trees
--   http://apfelmus.nfshost.com/articles/monoid-fingertree.html
-- ==========================================================
-- Dan Piponi, Haskell Monoids and their Uses
--   http://blog.sigfpe.com/2009/01/haskell-monoids-and-their-uses.html
-- ==========================================================
-- Data.Monoid documentation
--   http://haskell.org/ghc/docs/latest/html/libraries/base/Data-Monoid.html
-- ==========================================================
-- Data.Foldable documentation
--   http://haskell.org/ghc/docs/latest/html/libraries/base/Data-Foldable.html
--
-- ==========================================================



-------------------------------------------------------------------------------
-- Wacky boilerplate to make all tests run.
return []
-- run this to test all quickcheck properties
runTests :: IO Bool
runTests = $quickCheckAll
