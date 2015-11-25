-- Functors
-- http://www.seas.upenn.edu/~cis194/spring13/lectures/09-functors.html

-- Suggested Reading
-- LYAH - http://learnyouahaskell.com/making-our-own-types-and-typeclasses#the-functor-typeclass
-- http://www.haskell.org/haskellwiki/Typeclassopedia
--

-- Kinds
{-
Prelude> :k Int
Int :: *

In fact, every type which can actually serve as the type of some values
has kind *.

Prelude> :k Maybe
Maybe :: * -> *

Maybe is, in a sense, a function on types — we usually call it a type
constructor.
Maybe takes as input types of kind *, and produces another type of kind *.
For example, it can take as input Int :: *
 and produce the new type Maybe Int :: *.

Are there other type constructors with kind * -> * ?
Sure:
Prelude> :k []
vs
Prelude :k [] Int

Prelude> :k Tree
Tree :: * -> *

Function Types:
Prelude> :k (->)
(->) :: * -> * -> *

-}

-- Motivation for Functor
-- ======================
-- Note that the name “functor” comes from category theory, and is not the
-- same thing as functors in C++ (which are essentially first-class functions).
--
-- Over the past weeks we have seen a number of functions designed to “map”
-- a function over every element of some sort of container.
-- For example:
-- 1. map :: (a -> b) -> [a] -> [b]
-- 2. treeMap :: (a -> b) -> Tree a -> Tree b
-- 3. maybeEval :: (ExprT -> Int) -> Maybe ExprT -> Maybe Int
--    (homework 5 many people ended up doing a similar thing when you had to
--     somehow apply eval :: ExprT -> Int to a Maybe ExprT in order to get a
--     Maybe Int.)
-- 4. maybeMap :: (a -> b) -> Maybe a -> Maybe b
--

-- Now there is a repeated pattern here, so as good programmers we want to
-- know how to generalize it!
-- So what parts are the same, what parts are different?
-- The part that is different:
--  1. the container being "mapped over", which is of kind * -> *
--
class Functor' f where
  fmap :: (a->b) -> f a -> f b
-- so we are mapping:
--    a function (of kind: * -> * -> *)  - the first argument to fmap
--    over a container (of kind * -> *)  - represented by functor f - second argument and the result

-- Now we can just implement this class in a way specific to each particular f.
-- Note that the Functor class abstracts over types of kind * -> *.
--
-- e.g.
instance Functor' Maybe where
  fmap _ Nothing = Nothing
  fmap h (Just a) = Just (h a)
instance Functor' [] where
  fmap = map

-- What about IO?
-- :k IO
-- fmap :: (a -> b) -> IO a -> IO b
--   results in the IO action which first runs the IO a action,
--   then applies the function to transform the result before returning it.
instance Functor' IO where
  fmap f ioa = ioa >>= (return . f)
  -- simplification of
  -- fmap f ioa = ioa >>= (\x -> return (f x))
{-
Main.fmap (*2) (putStrLn "enter Number to be Doubled: " >> readLn)
-}

-- Now something mind-twisting!
instance Functor' ((->) e) where
  fmap = (.)
-- here container of kind *->* is (->) e
-- hence
--   fmap :: (a->b) -> f a -> f b
--   fmap :: (a->b) -> (->) e a -> (->) e b
-- OR fmap :: (a->b) -> (e -> a) -> (e -> b)

-- one way to think of a value of type (e -> a) is as a “e-indexed container”
--  with one value of a for each value of e
-- To map a function over every value in such a container corresponds
--  exactly to function composition:
--   to pick an element out of the transformed container, we first we apply
--   the (e -> a) function to pick out an a from the original container,
--   and then apply the (a -> b) function to transform the element we picked.
