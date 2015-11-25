-- http://www.seas.upenn.edu/~cis194/fall14/hw/04-poly.pdf
module HW04 where

{-
**Parametricity** **Parametric Polymorphism**
Haskell comes with a wonderful feature called parametric polymorphism,
which allows programmers to write datatypes and functions
that operate over a range of different types. Other languages support
similar features, such as C++’s templates, Java’s generics, and
OCaml’s parametric polymorphism (which is ever-so-slightly different
than Haskell’s!)

In these exercises, I will be providing a type, and you have to
provide an implementation—any implementation—that typechecks.
The only rule is that your implementation must be total. That is,
given any input, your implementations must always run in a finite
time and return a value. This means that any of the following are
disallowed:
• Infinite recursion
• Pattern-matching that does not contain all cases
• undefined
• error
• Any function beginning with unsafe.
• The use of non-total functions, such as head, tail, init, etc.

This list is not complete, but you will generally write total functions
unless you’re trying to be devious. (Or accidentally write a loop.)
After writing your implementation for each exercise, write a comment
describing the nature of all implementations of functions with
that type. In particular, if the number of functions that can inhabit
the type is finite, say what it is. If it’s not finite, try to come up with
properties of the functions that you can read right from the type.

-}
-- Exercise 1
ex1 :: a -> b -> b
ex1 _ b = b
-- only possible
-- has to return a value of same type as second argument,
-- we cant manufacture a value in b from a and b in a total manner

-- Exercise 2
ex2 :: a -> a -> a
ex2 l r = l
-- or r, (2) choices
-- to make other choices in a total manner impossible

-- Exercise 3
ex3 :: Int -> a -> a
ex3 n x = x
-- can check some property of Int but thats about it no other operation
-- in a total manner

-- Exercise 4
ex4 :: Bool -> a -> a -> a
ex4 f x y = if (f) then x else y
-- 4 possible ways
--  (1) above
--  (2) if (!f) then x else y
--  (3) ex1 x y
--  (4) ex1 y x
-- Your answer must include information on how many distinct functions
-- inhabit this type.

-- Exercise 5
ex5 :: Bool -> Bool
ex5 f = not f
-- 2 possible ways to hard code return and True/False
-- 2 possible ways to return f and not f
-- Your answer must include information on how many distinct functions
-- inhabit this type.

-- Exercise 6
ex6 :: (a -> a) -> a
ex6 = error("unable to give a distinct implementation here.")

-- Exercise 7
ex7 :: (a -> a) -> a -> a
ex7 f x = f x
-- 2 ways
-- (1) return x as is
-- (2) return the result of f applied over x


-- Exercise 8
ex8 :: [a] -> [a]
ex8 xs = xs
-- one possible implementation is to return arg as is
-- other ways is to do list operations which are available in a total way

-- Exercise 9
ex9 :: (a -> b) -> [a] -> [b]
ex9 f xs = map f xs
-- f must be applied to the input list (full or part) to return result of b

-- Exercise 10
ex10 :: Maybe a -> a
ex10 Nothing = error("No implemenation possible for this")
ex10 (Just x) = x
-- only partial implementation possible

-- Exercise 11
ex11 :: a -> Maybe a
ex11 x = Just x
-- only 1 implemenation possible totally
-- maynot be totally right a really solid total final implemenation is not possible
-- e.g for numbers, 1/0 should be Nothing and not Just Nan or Just Infinity

-- Exercise 12
ex12 :: Maybe a -> Maybe a
ex12 m = m
-- only 1 implemenation possible totally
