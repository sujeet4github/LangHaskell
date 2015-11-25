{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

-- More polymorphism and type classes
-- http://www.seas.upenn.edu/~cis194/spring13/lectures/05-type-classes.html

--  Haskell types are erased by the compiler after being checked:
-- at runtime, there is no type information around to query!
-- so not typeOf / instanceOf etc functions are available.

-- Type Classes
-- used on LHS of :: of a type declaration
-- standard:
--
-- Ord - types whose elements can be totally ordered
-- that is, where any two elements can be compared to see which
--  is less than the other.
-- It provides comparison operations like (<) and (<=), and also the compare function.
--
-- Num - is for “numeric” types, which support things like addition, subtraction,
-- and multipication.
--
-- Integral represents whole number types such as Int and Integer.
--
-- Show defines the method show, which is used to convert values into Strings.
--
-- Read is the dual of Show.
--

-- Making your own type classes example:
--
-- this does not seem to compile
--
