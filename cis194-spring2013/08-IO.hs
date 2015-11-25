{-# OPTIONS_GHC -Wall #-}

-- IO
-- http://www.seas.upenn.edu/~cis194/spring13/lectures/08-IO.html

-- Suggested Reading
-- chapter 9 of LYAH - http://learnyouahaskell.com/input-and-output
-- RWH - Chapter 7 - http://book.realworldhaskell.org/read/io.html
--

-- Record Syntax
--
-- Suppose we have a data type such as
--
newtype T1 = T1 Integer
newtype T2 = T2 Integer
newtype T3 = T3 Integer
data D' = C' T1 T2 T3
-- We could also declare this data type with record syntax as follows:
data D = C { field1 :: T1, field2 :: T2, field3 :: T3 }
-- here we specify not just a type but also a name for each field stored
-- inside the C constructor. This new version can be used in all the
-- same ways -
-- In particular,
--   we can still construct and pattern-match on values of type D
--   as (C v1 v2 v3)
-- Additional Benefits:
-- 1. each field name is automatically a projection function, an 'getter'
-- e.g.
--  field2 :: D -> T2
-- 2. special syntax for constructing
--      C { field3 = ..., field1 = ..., field2 = ... }
--    or create a new value from an existing value, like
--      d { field3 = ... }, where d :: D
--    and pattern-matching
--    foo (C { field1 = x }) = ... x ...
--     i.e. match only on field1, ignoring the others that are not needed
--

-- The IO type
--
-- Values of type IO a are descriptions of effectful computations, which, if
-- executed would (possibly) perform some effectful I/O operations
--  and (eventually) produce a value of type a.
--
-- NOTE:  A value of type IO a, in and of itself, is just an inert,
--  perfectly safe thing with no effects. It is just a description of an
-- effectful computation.
-- e.g. recipe for a cake
--   just holding a recipe in your hands does nothing.
--   to actually produce a cake, the recipe must be followed/executed.
--
-- So how do the values of type IO get exected?
-- only one way!
-- the Haskell compiler looks for a special value
-- main :: IO ()
-- which will actually get handed to the runtime system and executed.
-- That’s it!
--  Think of the Haskell runtime system as a master chef who is the only
-- one allowed to do any cooking.
--
-- If you want your recipe to be followed then you had better make it part of
-- the big recipe (main) that gets handed to the master chef.
-- Of course, main can be arbitrarily complicated, and will usually be composed
-- of many smaller IO computations.
--

-- Combining IO
-- ------------
-- 1. And Then   (>>)
-- (>>) :: IO a :: IO b :: IO b
-- e.g
--  main = putStrLn "Hello" >> putStrLn "world!"

-- This works fine for code of the form “do this; do this; do this” where the
-- results don’t really matter.

-- What if we dont want to throw away the result from the first computation?
-- What if we want the result of the second computation to be able to depend
-- on the result of the first?

-- 2. Bind   (>>=)
-- (>>=) :: IO a -> (a -> IO b) -> IO b
-- Bind takes
--   a computation that will produce a value of type a
--   a function that gets to compute a second computation based on this
--    intermediate value of type a
-- Bind results in a computation that will
--   1. perform the first computation
--   2. uses its result ro decide what to do and then do that
-- e.g
-- main = putStrLn "Enter a number: " >> (readLn >>= (\n -> putStrLn (show (n+1))))
