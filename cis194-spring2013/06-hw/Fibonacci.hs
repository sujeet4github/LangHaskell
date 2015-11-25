{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}

import Test.QuickCheck
import Test.QuickCheck.All()
import Data.List(foldl')

{-
http://www.seas.upenn.edu/~cis194/spring13/hw/06-laziness.pdf

This week we learned about Haskell’s lazy evaluation. This homework
assignment will focus on one particular consequence of lazy
evaluation, namely, the ability to work with infinite data structures.

The Fibonacci numbers Fn are defined as the sequence of integers,
beginning with 0 and 1, where every integer in the sequence is the
sum of the previous two. That is,
F0 = 0
F1 = 1
Fn = Fn−1 + Fn−2 (n ≥ 2)

-}
-------------------------------------------------------------------------------
-- Exercise 1:
-- Translate the above definition of Fibonacci numbers directly into a
-- recursive function
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
  | n >= 2  = fib (n - 2) + fib (n - 1)
  | otherwise = fib (n + 2) - fib (n + 1)

-- Now use fib to define the infinite list of all Fibonacci numbers,
fibs1 :: [Integer]
fibs1 = map fib [0..]

-------------------------------------------------------------------------------
-- Exercise 2:
-- Although it is a good way to define the Fibonacci
-- numbers, it is not a very good way to compute them
-- Your task for this exercise is to come up with more efficient implementation.
-- computing the first n elements of fibs2 requires
-- only O(n) addition operations.

-- for +ve, fib a b = a + f (b, a+b), start with a = 0, b = 1
fibs2 :: [Integer]
fibs2 = fibPosGen 0 1
        where fibPosGen a b = a : fibPosGen b (a+b)

-- my contrived thinking determined to use fold's came up with the foll
--  using foldlefts
-- Specifically, define the infinite list: fibs2 :: [Integer]
-- so that it has the same elements as fibs1, but computing the first n
-- elements of fibs2 requires only O(n) addition operations. Be sure to
-- use standard recursion pattern(s) from the Prelude as appropriate
fibs2NotToBeUsedWithInfiniteLists :: [Integer]
fibs2NotToBeUsedWithInfiniteLists = foldl' fibLeftFolder [] [0..100]

fibLeftFolder :: (Num a, Ord a) => [a] -> a -> [a]
fibLeftFolder [] _ = [0]
fibLeftFolder [0] 1 = [1,0]
fibLeftFolder acc@(minus1 : (minus2 : _)) n | n > 1 = (minus2 + minus1) : acc
fibLeftFolder _ _ = undefined

-------------------------------------------------------------------------------
-- Streams
-- We can be more explicit about infinite lists by defining a type Stream
-- representing lists that must be infinite.
-- The list type has two constructors,
--   [] (the empty list)
--   and (:) (cons)
-- streams are like lists but with
--   only a “cons” constructor,
--   there is no such thing as an empty stream.

-- Exercise 3:
--
-- 1. Define a data type of polymorphic streams, Stream.
data Stream a = Cons a (Stream a)

-- To test your Stream functions in the succeeding exercises, it will be
-- useful to have an instance of Show for Streams. However, if you put
-- deriving Show after your definition of Stream, as one usually does,
-- the resulting instance will try to print an entire Stream—which,
-- of course, will never finish. Instead, you should make your own
-- instance of Show for Stream,

-- showing only some prefix of a stream (say, the first 20 elements)
instance Show a => Show (Stream a) where
    show = loopOn (20::Int) where
            loopOn 0 _ = " ..."
            loopOn n (Cons a rest) = show a ++ ", " ++ loopOn (n-1) rest

-- cool another way...
-- instance Show a => Show (Stream a) where
--    show x = show (take 20 $ streamToList x)

-- 2. Write a function to convert a Stream to an infinite list
streamToList :: Stream a -> [a]
streamToList (Cons a ss) = a : streamToList ss

-------------------------------------------------------------------------------
-- Exercise 4:
--
-- Let’s create some simple tools for working with Streams

-- generates a stream containing infinitely many copies of the given element.
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

-- applies a function to every element of a Stream.
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x rest) = Cons (f x) (streamMap f rest)

-- generates a Stream from a “seed” of type a, which is the
-- first element of the stream, and an “unfolding rule” of type a -> a
-- which specifies how to transform the seed into a new seed, to be
-- used for generating the rest of the stream.
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed f x = Cons x (streamFromSeed f (f x))

-- streamRepeat can be redefined as:
streamRepeat' :: a -> Stream a
streamRepeat' x = streamFromSeed id x

-------------------------------------------------------------------------------
-- Exercise 5:
--

-- infinite list of natural numbers
nats :: Stream Integer
nats = streamFromSeed (+ 1) 0

-- ruler function where
-- nth element in the stream (assuming first element is n=1)
-- is the largest power of 2 which evenly divides n
rulerCheck :: Integer -> Integer -> Bool
rulerCheck p n = 0 == (2 ^ p) `mod` (2 * n)

-- Hint: define a function interleaveStreams which alternates
-- the elements from two streams.
-- Can you use this function to implement
-- ruler in a clever way that does not have
-- to do any divisibility testing?
--
streamZip :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
streamZip f (Cons x xs) (Cons y ys) = Cons (f x y) (streamZip f xs ys)

interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)

-------------------------------------------------------------------------------
-- Exercise 6:
--
-- Fibonacci numbers via generating functions
-- We will use streams of Integers to compute the Fibonacci numbers in an
-- astounding way.
-- The essential idea is to work with generating functions of the form:
--  a0 * (x ^ 0) + a1 * (x ^ 1) +.... + an * (x ^ n) + ...
-- where x is just a "formal parameter"
-- (that is, we will never actually substitute any values for x;
--  we just use it as a placeholder)
--  and all the coefficients ai are integers.
-- We will store the coefficients a0, a1, a2, . . . in a Stream Integer.

-- y noting that x = 0 + 1x + 0x^2 + 0x^3 + ...

-- first define xVal by noting x = 0 + 1x + 0x^2 + 0x^3 + ...
--   dont know how this comes about??
zeroes :: Stream Integer
zeroes = streamRepeat 0
x' :: Stream Integer
x' = Cons 0 (Cons 1 zeroes)

-- Define an instance of the Num type class for Stream Integer
-- Once we do this, we should be able to evaluate folling at GHCI prompt:
-- x' ^ 4
--  (1 + x')^5
-- (x'^2 + x' + 3) * (x' - 5)
instance Num (Stream Integer) where
  -- implement fromInteger
  -- Note that n = n + 0x + 0x^2 + 0x^3 + ...
  --  dont know why ??
  fromInteger n = Cons n zeroes
  -- implement negate
  --   negate all coefficients of the generating function
  negate xs = streamMap negate xs
  -- implement (+)
  --   add up the coefficients of the generating function
  (+) xs ys = streamZip (+) xs ys
  -- implement (*)
  --- AB = (a0 + xA') B
  --     = a0 B + x A'B
  --     = a0 (b0 + xB') + x A'B
  --     = a0.b0 + x (a0.B' + A'B)
  (Cons a0 strA') * strB@(Cons b0 strB') = Cons (a0 * b0) (a0B' + (strA' * strB))
                    where
                      a0B' = streamMap (* a0) strB'
  -- we do not need these methods
  -- do this or {-# OPTIONS_GHC -fno-warn-missing-methods #-}
  abs = undefined
  signum = undefined

-- Define an instance of Fractional type class for Stream Integer
-- implement (/)
--- if A = a0 + xA' and B = b0 + xB'
-- then A / B = Q
--  where Q = (a0/b0) + x ( (A0 − QB0) / b0 )
--
instance Fractional (Stream Integer) where
  (Cons a0 strA') / (Cons b0 strB') = q
                    where
                      --- Q = (a0/b0) + x (A' - QB') magic lol
                      -- use the div operation where appropriate (when result may not be Integer)
                      divBy_b0 x0 = x0 `div` b0
                      -- divBy_b0 x0 = floor (fromIntegral x0 / fromIntegral b0::Double)
                      a0_Div_b0 = divBy_b0 a0
                      q = Cons a0_Div_b0 (streamMap divBy_b0 (strA' - (q * strB')))
  fromRational = undefined

-- Consider representing the Fibonacci numbers using a generating functions,
-- see the math in the homework site...
-- but it finally resolves to...
--  F(x) = x / (1 - x - x^2)
fibs3 :: Stream Integer
fibs3 = x' / (1 - x' - (x' ^ (2::Integer)))

-------------------------------------------------------------------------------
-- Exercise 7:
--
-- Fibonacci numbers via matrices

-- Create a type Matrix which represents 2 × 2 matrices of Integers
data Matrix2x2 = Matrix Integer Integer Integer Integer deriving Show

instance Eq Matrix2x2 where
  (Matrix r1c1 r1c2 r2c1 r2c2) == (Matrix r1c1' r1c2' r2c1' r2c2') =
    r1c1 == r1c1' &&
    r1c2 == r1c2' &&
    r2c1 == r2c1' &&
    r2c2 == r2c2'

-- Make an instance of the Num type class for Matrix. In fact, you only
-- have to implement the (*) method,
-- If you want to play around with matrix operations a bit
-- more, you can implement fromInteger, negate, and (+) as well.
instance Num Matrix2x2 where
  (Matrix r1c1 r1c2 r2c1 r2c2) * (Matrix r1c1' r1c2' r2c1' r2c2') =
    Matrix newR1C1 newR1C2 newR2C1 newR2C2
      where
        newR1C1 = r1c1*r1c1' + r1c2*r2c1'
        newR1C2 = r1c1*r1c2' + r1c2*r2c2'
        newR2C1 = r2c1*r1c1' + r2c2*r2c1'
        newR2C2 = r2c1*r1c2' + r2c2*r2c2'
  (+) = undefined
  fromInteger = undefined
  negate = undefined
  abs = undefined
  signum = undefined

test_2x2MatMul_ex1 :: Bool
test_2x2MatMul_ex1 = let
                        l = Matrix 1 1 1 0
                        r = Matrix 1 1 1 0
                    in l * r == Matrix 2 1 1 1
--
test_2x2MatMul_ex2 :: Bool
test_2x2MatMul_ex2 = let
                        z = Matrix 1 1 1 0
                        o = Matrix 1 1 1 0
                    in (o * z) * z == Matrix 3 2 2 1
--
nthPowerOfF :: Matrix2x2 -> Integer -> Matrix2x2
nthPowerOfF _ 0 = Matrix 0 0 0 0
nthPowerOfF f 1 = f
nthPowerOfF f n = f * (nthPowerOfF f (n - 1))

test_nthPowerOfF :: Bool
test_nthPowerOfF = expected == actual
                    where
                      expected = Matrix 8 5 5 3
                      actual = nthPowerOfF fibStartMatrix 5

fibStartMatrix :: Matrix2x2
fibStartMatrix = Matrix 1 1 1 0

extractFibonacci :: Matrix2x2 -> Integer
extractFibonacci (Matrix _ r1c2 _ _) = r1c2


-- We now get fast (logarithmic time) matrix exponentiation for free,
-- since (^) is implemented using a binary exponentiation algorithm
-- in terms of (*)  - requires only O(log n) multiplications
--
fibs4 :: Integer -> Integer
fibs4 0 = 0
fibs4 n = extractFibonacci nthF
          where
            nthF = fibStartMatrix ^ n


-------------------------------------------------------------------------------
-- Wacky boilerplate to make all tests run.
return []
-- run this to test all quickcheck properties
runTests :: IO Bool
runTests = $quickCheckAll
