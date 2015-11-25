-- http://learnyouahaskell.com/types-and-typeclasses
--

{-
 GHCI :t command

 doing :t on an expression prints out the expression followed by :: and its type.
 :: is read as "has type of".

 Explicit types are always denoted with the first letter in capital case.


Common types:
1. Int stands for integer. It's used for whole numbers. Int is bounded, which means that
    it has a minimum and a maximum value.
    Usually on 32-bit machines the maximum possible Int is 2147483647
       and the minimum is -2147483648.
2. Integer stands for, er … also integer.
    The main difference is that it's not bounded so it can be used to represent really
    really big numbers. Int, however, is more efficient.
3. Float is a real floating point with single precision
4. Double is a real floating point with double the precision!
5. Bool is a boolean type. It can have only two values: True and False
6. Char represents a character. It's denoted by single quotes. A list of characters is a string.
* Tuples are types but they are dependent on their length as well as the types of their components.
Note that the empty tuple () is also a type which can only have a single value: ()
-}



-- Type Variables

{-
This is much like generics in other languages, only in Haskell it's much more powerful
because it allows us to easily write very general functions if they don't use any specific
behavior of the types in them.

Functions that have type variables are called polymorphic functions.

The type declaration of head states that it takes a list of any type and returns one element of that type.
:t head
head :: [a] -> a

:t fst
fst :: (a, b) -> a
fst takes a tuple which contains two types and returns an element which is of the same type as
the pair's first component. Thats the reason fst only works on a 2 pair tuple.

-}



-- Typeclasses 101

{-
A typeclass is a sort of interface that defines some behavior.

If a type is a part of a typeclass, that means that it supports and implements the behavior the
typeclass describes.

Basic Typeclasses:
1. Eq is used for types that support equality testing.
    The functions its members implement are == and /=.
2. Ord is for types that have an ordering.
    The compare function takes two Ord members of the same type and returns an ordering.
    Ordering is a type that can be GT, LT or EQ.
3. Members of Show can be presented as strings.
    The most used function that deals with the Show typeclass is show.
    It takes a value whose type is a member of Show and presents it to us as a string.
4. Read is sort of the opposite typeclass of Show.
    The read function takes a string and returns a type which is a member of Read.
5. Enum members are sequentially ordered types — they can be enumerated.
    The main advantage of the Enum typeclass is that we can use its types in list ranges.
    They also have defined successors and predecesors, which you can get with the
    succ and pred functions.
    Types in this class: (), Bool, Char, Ordering, Int, Integer, Float and Double
6. Bounded members have an upper and a lower bound.
    minBound :: Int
    minBound :: Bool
    minBound :: Char
    maxBound :: Int
    maxBound :: Bool
    maxBound :: Char
    All tuples are also part of Bounded if the components are also in it.
    maxBound :: (Bool, Int, Char)
7. Num is a numeric typeclass. Its members have the property of being able to act like numbers.
    types in Num are {Int, Integer, Float, Double}
    To join Num, a type must already be friends with Show and Eq
8. Integral is also a numeric typeclass.
    Integral includes only integral (whole) numbers. In this typeclass are Int and Integer.
9. Floating includes only floating point numbers, so Float and Double
-}

ord_ex_1 = "Abrakadabra" < "Zebra"
ord_ex_2 = "Abrakadabra" `compare` "Zebra"

read_ex_1 = read "[1,2,3,4]" ++ [3]
read_ex_2 = read "False" || True
read_ex_3 = read "8.2" + 3.8

-- Most expressions are such that the compiler can infer what their type is by itself.
-- But sometimes, the compiler doesn't know whether to return a value of type Int or Float
-- for an expression like read "5"
-- Haskell is a statically typed language,
-- it has to know all the types before the code is compiled (or in the case of GHCI, evaluated).
-- so, we use explicit type annotations.
-- Type annotations are a way of explicitly saying what the type of an expression should be.
-- We do that by adding :: at the end of the expression and then specifying a type.
read_ex_4 = (read "5" :: Float) * 4
read_ex_5 = read "(3, 'a')" :: (Int, Char)


-- A very useful function for dealing with numbers is fromIntegral.
-- It has a type declaration of fromIntegral :: (Num b, Integral a) => a -> b.
-- From its type signature we see that it takes an integral number
--      and turns it into a more general number.
--      That's useful when you want integral and floating point types to work together nicely.
using_fromIntegral = fromIntegral (length [1,2,3,4]) + 3.2
