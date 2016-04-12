module HW07 where

import System.Random

{-
Ex 1 - Fibonacci numbers
-}
fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib n
    | n > 1 = fib (n - 1) + fib (n - 2)
    | otherwise = error "Fibonacci cannot be determined"

fibsl :: [Integer]
fibsl = fmap fib [0..]

{-
Ex 2: more efficient way to generate fibonacci
complex recursion ....
zipWith applies an operation to each element between both lists. (upto smaller of two.)
-}

fibs2 :: [Integer]
fibs2 = [0,1] ++ zipWith (+) fibs2 (tail fibs2)

{-
Extra Credit: Ex 11:
computing nth fibonacci with only O(log n) arithmetic operations
-}
-- 11.1 create type which represents 2 x 2 matrices of Integers
data Mat2x2 = Mat2x2 {  zz :: Integer, zo :: Integer,
                        oz :: Integer, oo :: Integer
    } deriving (Show, Eq)

-- 11.2 Make an instance of Num type for Mat2x2
-- only need to implement * method
instance Num Mat2x2 where
    (Mat2x2 x11 x12 x21 x22) * (Mat2x2 y11 y12 y21 y22) = (Mat2x2 z11 z12 z21 z22)
        where
            z11 = x11*y11 + x12*y21
            z12 = x11*y12 + x12*y22
            z21 = x21*y11 + x22*y21
            z22 = x21*y12 + x22*y22
    fromInteger n = Mat2x2 n 0 0 n

-- Doing this (*) function gets us fast (log time) matrix exponentiation for free
-- since ^ is implemented using a binary exponentiation algorithm in terms of (*).
-- 11.3
fib4 :: Integer -> Integer
fib4 n = zz f_raised_to_power_n
    where
        f1 = Mat2x2 1 1 1 0
        f_raised_to_power_n = f1 ^ n


{-
We can be more explicit about infinite lists by defining
a Stream. A list has two constructors, empty list and cons,
there is no such thing as an empty stream.
So a stream is defined as an element followed by a stream.
-}
data Stream a = Cons a (Stream a)

{-
Ex 3: convert a stream to an infinite list
-}
streamToList :: Stream a -> [a]
streamToList (Cons a rest) = [a] ++ streamToList rest

{-
Ex 4: Show for stream
To test your Stream functions in the succeeding exercises,
it will be useful to have an instance of Show for Streams.
However, if you put deriving Show after your definition of
Stream, as one usually does, the resulting instance will
try to print an entire Stream — which, of course, will
never finish. Instead, make your own instance of Show for
Stream that shows some prefix (20) of a stream
-}
instance Show a => Show (Stream a) where
    show s = "First 20 of stream ... " ++ (show $ take 20 $ streamToList s)

listToStream (h:t) = (Cons h (listToStream t))
test_1=listToStream [1,20..]

{-
Ex 5: Let’s create some simple tools for working with Streams.

-}

-- Ex. 5.1
streamRepeat :: a -> Stream a
streamRepeat x = Cons x (streamRepeat x)

test_2=streamRepeat 10

-- Ex. 5.2
streamMap :: (a -> b) -> Stream a -> Stream b
streamMap f (Cons x xs) = Cons (f x) (streamMap f xs)

-- Ex. 5.3
-- which generates a Stream from a *seed* of type a, which is the
-- first element of the stream, and an *unfolding rule* of type
-- a -> a which specifies how to transform the seed into a new
-- seed, to be used for generating the rest of the stream
streamFromSeed :: (a -> a) -> a -> Stream a
streamFromSeed sf s = Cons s (streamFromSeed sf new_s)
    where
        new_s = sf s

test_3=lhs == rhs
    where
        lhs = take 5 $ streamToList (streamFromSeed ('x' :) "o")
        rhs = ["o", "xo", "xxo", "xxxo", "xxxxo"]


{-
Ex 6: Creating a few streams
-}

-- Ex. 6.1
-- natural numbers
nats :: Stream Integer
nats = streamFromSeed (+1) 0

-- Ex. 6.2
-- ruler function
-- where the nth element in the stream (assuming the first element
-- corresponds to n = 1) is the largest power of 2 which evenly
-- divides n.

get_highestPowerOf2 :: Integer -> Integer
get_highestPowerOf2 n
    | n > 1 = (quot n 2)
    | otherwise = 0

check_power :: Integer -> Integer -> Bool
check_power 1 0 = True
check_power x n
    | n >= 1    = mod x (2 ^ n) == 0
    | otherwise = False

ruler_fn :: Integer -> Integer
ruler_fn n = ruler_fn_with_acc (get_highestPowerOf2 n) n
    where
        ruler_fn_with_acc acc n
            | acc > 0   = if check_power n acc then acc else ruler_fn_with_acc (acc - 1) n
            | otherwise = 0

ruler_with_div = streamMap ruler_fn whole_numbers
    where
        whole_numbers = streamFromSeed (+1) 1

{-
Hint: define a function
interleaveStreams which alternates
the elements from two streams. Can
you use this function to implement
ruler in a clever way that does not have
to do any divisibility testing?

Another Hint: You will want
interleaveStreams to be lazy in its
second parameter. Why?
-}

ruler :: Stream Integer
ruler = foldr1 interleaveStreams (map streamRepeat [0..])

--helper function
interleaveStreams :: Stream a -> Stream a -> Stream a
interleaveStreams (Cons x xs) ys = Cons x (interleaveStreams ys xs)


{-
Section 2:
1. Random Numbers
2. Profiling
    It’s wonderful to be lazy, but laziness occasionally gets in the way of
    productive work.
-}

-- Ex 7
randomList :: (Random a, RandomGen g) => g -> [a]
randomList g = n : randomList g2
    where
        (n,g2)=random g

-- Ex 8
randomInts :: Int -> [Int]
randomInts n = take n $ randomList (mkStdGen 10)

minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing -- no min or max if there are no elements
minMax xs = Just (minimum xs, maximum xs)

{- Exercise 9

See the week 7 lecture notes for more details about profiling.

Use minMax to find the minimum and maximum of a pseudo-random sequence of 1,000,000 Ints.
Then, print out these values from a main action.

Now, compile your program, enabling RTS options
: ghc HW07.hs -rtsopts -main-is HW07

(GHC normally requires that your main action be in a module named Main.
However, this would cause havoc with our autograde system, and so we’re
asking that your homework be in a module named HW07. To tell GHC to
run the main function from the HW07 module—not the Main module—you say
-main-is HW07.)

and run your program to see how much memory it takes.
: ./HW07 +RTS -s
or
: HW07.exe +RTS -s on Windows)
It should be a lot.

Record the “total memory in use” figure in a comment in your source file.

Then, run your program to see its heap profile, like this:
> ./HW07 +RTS -h -i0.001
> hp2ps -c HW07.hp
(or, for Windows users running at the Windows command prompt cmd.exe:
> HW07.exe +RTS -h -i0.001
> hp2ps -c HW07.hp
)
This will create a HW07.ps file, which can be viewed by most modern
PDF readers. Check it out. Include this HW07.ps file with your
submission.

-}

main = print results
    where
--        results = minMax $ randomInts 1000000
-- using minMax: 302 MB total memory in use
        results = minMax2 $ randomInts 1000000
-- using minMax2: 855 MB total memory in use


{- Exercise 10

As written, minMax does not take advantage of Haskell’s
laziness, because it calculates the maximum of xs and the minimum
of xs separately. The running program must remember all of xs
between these calculations. But, with a rewrite, minMax can calculate
both the minimum and maximum on the fly, and your program will
never need to store the whole list
-}
minMax2 :: [Int] -> Maybe (Int, Int)
minMax2 [] = Nothing -- no min or max if there are no elements
minMax2 (x:xs) = Just $ foldr minmaxfn (x, x) xs
    where
        minmaxfn x (mn,mx) = ((min mn x), (max mx x))


