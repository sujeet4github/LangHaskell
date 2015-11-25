-- The Monoid Type Class

import Test.QuickCheck

import Data.Monoid

-- Also is CIS194 course folder - lesson 07
--

-- When making a type (new type that we define) a Monoid, it suffices
-- to implement mempty and mappend.
-- the default implementation of mconcat is just fine, it is changed
-- only to make it more efficient
--


-- Monoid Laws:
-- Identity
--  mempty `mappend` x	= x
--  x `mappend` mempty	= x
-- Associative
--  (x `mappend` y) `mappend` z		= x `mappend` (y `mappend` z)


-- Products/Sums are Monoids
-- TODO:
-- QuickCheck tests to test Monoidal laws
prop_sum_leftIdentity :: Sum Integer -> Bool
prop_sum_leftIdentity sa = 0 <> sa == sa

-- teach QuickCheck how to generate arbitrary Sum's
instance Arbitrary (Sum a) where
--	coarbitrary = undefined
	arbitrary = oneof [0..10]

-- Test [1,2,3] `mappend` [4,5,6]
-- Test Monoid Laws on List
prop_list_leftIdentity :: Eq t => [t] -> Bool
prop_list_leftIdentity xs = [] <> xs == xs
prop_list_rightIdentity :: Eq t => [t] -> Bool
prop_list_rightIdentity xs = [] <> xs == xs
prop_list_associativity xs ys zs = (xs <> ys) <> zs == xs <> (ys <> zs)
