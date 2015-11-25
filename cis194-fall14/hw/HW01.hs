{-
http://www.seas.upenn.edu/~cis194/fall14/hw/01-intro.pdf

Name: <your name here>
Collaborators: <your collaborators here, or "none">
Notes: <any particular notes about your work -- what you struggled with,
        what's not working, what's really cool, etc.>
-}

module HW01 where         -- We'll learn more about this later

isThisWorking :: String
isThisWorking = "Yes"
-- Load this file into GHCi (say, with `ghci HW01.hs`) and type
-- `isThisWorking` at the prompt. GHCi will tell you whether it's working!

-- Put your work below.

-- Exercise 1 We first need to be able to break up a number into its last
-- digit and the rest of the number
lastDigit :: Integer -> Integer
lastDigit x = rem x 10

dropLastDigit :: Integer -> Integer
dropLastDigit x = div x 10

-- Exercise 2 Now, we can break apart a number into its digits.
toDigits :: Integer -> [Integer]
toDigits x
    | x > 10    = (toDigits . dropLastDigit) x ++ [lastDigit x]
    | x > 0     = [x]
    | otherwise = []

-- Exercise 3 Once we have the digits in the proper order, we need to
-- double every other one (from the right ... that is, the second-to-last,
-- fourth-to-last,. . . numbers are doubled
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleEveryOtherFromLeft . reverse
    where
        doubleEveryOtherFromLeft [] = []
        doubleEveryOtherFromLeft [x] = [x]
        doubleEveryOtherFromLeft (x:y:xs) = x:(2*y):(doubleEveryOtherFromLeft xs)

-- Exercise 4 The output of doubleEveryOther has a mix of one-digit
-- and two-digit numbers. calculate the sum of all digits
sumDigits :: [Integer] -> Integer
sumDigits xs = foldr (\x a -> x+a) 0 (makeSingleDigits xs)
    where
        makeSingleDigits xs = concat (map toDigits xs)

-- Exercise 5 Define the function that indicates whether an Integer
-- could be a valid credit card number.
-- This will use all functions defined in the previous exercises.
validate :: Integer -> Bool
validate = (== 0) . lastDigit . sumDigits . doubleEveryOther . toDigits

{-
Exercise 6

The Towers of Hanoi is a classic puzzle with a solution that can be
described recursively. Disks of different sizes are stacked on three pegs;
the goal is to get from a starting configuration with all disks stacked
on the first peg to an ending configuration with all disks stacked on
the last peg, as shown in Figure 1.

The only rules are
. you may only move one disk at a time, and
. a larger disk may never be stacked on top of a smaller one

For example, as the first move all you can do is move the topmost,
smallest disk onto a different peg, since only one disk may be moved
at a time.

To move n discs (stacked in increasing size) from peg a to peg b
using peg c as temporary storage,
1. move n - 1 discs from a to c using b as temporary storage
2. move the top disc from a to b
3. move n - 1 discs from c to b using a as temporary storage.

-}

type Peg = String
type Move = (Peg, Peg)
hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
-- logic specified above...
hanoi n a b c =
    hanoi (n-1) a c b
    ++ [(a,b)]
    ++ (hanoi (n-1) c b a)

hanoi4 :: Integer -> Peg -> Peg -> Peg -> Peg -> [Move]
hanoi4 0 _ _ _ _ = []
-- TODO
-- This does not work,
-- refer to https://en.wikipedia.org/wiki/Tower_of_Hanoi#Frame-Stewart_algorithm
--
hanoi4 n a b c d =
    let
        one = (n-1) `div` 2
        two = (n - one - 1)
    in
        hanoi4 one a c b d
        ++ hanoi4 two a d b c
        ++ [(a,b)]
        ++ (hanoi4 two d b a c)
        ++ (hanoi4 two c b a d)
