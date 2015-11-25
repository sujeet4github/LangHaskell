-- http://learnyouahaskell.com/functors-applicative-functors-and-monoids#monoids
--

{-
Type classes in Haskell are used to present an interface for types that have some
behavior in common. We started out with simple type classes like Eq, which is for
types whose values can be equated, and Ord, which is for things that can be put in
an order and then moved on to more interesting ones, like Functor and Applicative.

Another behavior is when you have an associative binary function and a value which
acts as an identity with respect to that function.
This is captured in a Monoid.
-}
import Prelude hiding (Monoid, mappend, mempty, mconcat)

class Monoid m where
    -- represents the identity value
    mempty :: m
    -- represents the binary function (unfortunate choice of name!)
    mappend :: m -> m -> m
    -- reduces a list of values to a single value
    mconcat :: [m] -> m
    -- default implementation is fine for most implementations
    mconcat = foldr mappend mempty
-- Only concrete types can be made instances of Monoid, because the m in the type
-- definition does not take any type parameters.

-- Monoid laws
-- 1. identity from left
-- mempty `mappend` x = x
-- 2. identity from right
-- x `mappend` mempty = x
-- 3. associative
-- (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

-- Examples of Monoids
-- 1. two ways for numbers to be Monoids
--    newtype is used to define these: Product and Sum
-- 2. arrays, with ++ and []
-- 3. two ways for bools to be Monoids
--     Any (||) and All (&&) - again newtype is used here
-- 4. Ordering type is also defined as a monoid w.r.t compare operation
--   The Ordering monoid is very cool because it allows us to easily compare
--   things by many different criteria and put those criteria in an order themselves,
--   ranging from the most important to the least.
-- 5. Maybe
--      if the underlying value is a Monoid, with Nothing as the identity
--      if underlying is not a Monoid, The First (getting the first non-nothing) op can
--        be a monoid, with Nothing as identity
--      defined in Data.Monoid.First
newtype First a = First { getFirst :: Maybe a }
    deriving (Eq, Ord, Read, Show)
instance Monoid (First a) where
    mempty = First Nothing
    First (Just x) `mappend` _ = First (Just x)
    First Nothing `mappend` x = x
-- First is useful when we have a bunch of Maybe values and we just want to know if
-- any of them is a Just.
anyNonNothing = let testList = [Nothing, Just 9, Just 10]
                    in getFirst . mconcat . map First $ testList
-- similiar, in functionality is, Data.Monoid.Last
--
-- 6. Using Monoids to fold data structures
--      Because there are so many data structures that work nicely with folds, the
--       Foldable type class was introduced.
--      Much like Functor is for things that can be mapped over,
--       Foldable is for things that can be folded up!
--
-- See Tree.hs