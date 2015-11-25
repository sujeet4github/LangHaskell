-- https://hackage.haskell.org/package/QuickCheck-2.8.1/docs/Test-QuickCheck-All.html
-- to use QuickCheck.All
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All


-- http://www.seas.upenn.edu/~cis194/spring13/lectures/01-intro.html
--

-- Defining basic functions

-- 1. by cases
-- Each clause is checked in order from top to bottom,
--  and the first matching clause is chosen.

-- Compute the sum of the integers from 1 to n.
sumtorial :: Integer -> Integer
sumtorial 0 = 0
sumtorial n = n + sumtorial (n-1)

-- 2. guards - choices made using boolean expressions,
--   the n case has 2 guards.
--  Any number of guards can be associated with each case.
--
hailstone :: Integer -> Integer
hailstone 0 = 0
hailstone n
  | n `mod` 2 == 0 = n `div` 2
  | otherwise     = 3*n +1

prop_hailstone_0 = 0 == (hailstone 0)
prop_hailstone_div_by2 = 1 == (hailstone 2)
prop_hailstone_NotDiv_by2 = 10 == (hailstone 3)
prop_hailstone n =
  if ((n `mod` 2) == 0)
    then calc_val1 == actual_val
    else calc_val2 == actual_val
      where calc_val2 = 3*n+1
            calc_val1 = n `div` 2
            actual_val = hailstone n


-- Homework
--
-- http://www.seas.upenn.edu/~cis194/spring13/hw/01-intro.pdf

-- implement the validation algorithm for credit cards. It follows these steps
-- step 1. Double the value of every second digit beginning from the right.
--         That is, the last digit is unchanged; the second-to-last digit is doubled;
--         the third-to-last digit is unchanged; and so on. For example, [1,3,8,6] becomes [2,3,16,6].
-- step 2. Add the digits of the doubled values and the undoubled digits from the original number.
--         For example, [2,3,16,6] becomes 2+3+1+6+6 = 18
-- step 3. Calculate the remainder when the sum is divided by 10. For the above example, the remainder would be 8.
-- If the result equals 0, then the number is valid.

-- 1. find digits of a number
toDigits :: Integer -> [Integer]
toDigits n = reverse $ toDigitsRev n

toDigitsRev :: Integer -> [Integer]
toDigitsRev 0 = []
toDigitsRev n
  | n < 0   = []
  | n < 10  = [n]
  | otherwise  = (n `mod` 10) : toDigitsRev (n `div` 10)

prop_toDigits_ex1 = toDigits 1234 == [1,2,3,4]
prop_toDigits_ex2 = toDigitsRev 1234 == [4,3,2,1]
prop_toDigits_ex3 = toDigits 0 == []
prop_toDigits_ex4 = toDigits (-17) == []

-- 2. Once we have the digits in the proper order, we need to double every other one
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther []   = []
doubleEveryOther [n]  = [n]
doubleEveryOther (n:ns)
    | length ns `mod` 2 == 1  = 2*n : (doubleEveryOther ns)
    | otherwise               = n : (doubleEveryOther ns)

prop_doubleEveryOther_ex1   = doubleEveryOther [8,7,6,5] == [16,7,12,5]
prop_doubleEveryOther_ex2   = doubleEveryOther [1,2,3] == [1,4,3]
prop_doubleEveryOther_ex3   = doubleEveryOther [] == []
prop_doubleEveryOther_ex4   = doubleEveryOther [1] == [1]

-- 3. calculate sum of digits
-- Note: The output of doubleEveryOther has a mix of one-digit and two-digit numbers
sumDigits :: [Integer] -> Integer
sumDigits []    = 0
sumDigits (n:ns)
  | n < 10    = n + sumDigits ns
  | otherwise   = sumDigits (toDigits n) + sumDigits ns

prop_sumDigits_ex1 =    sumDigits [16,7,12,5] == 22


-- 4. define the validate function
-- indicates whether an Integer could be a valid credit card number. This will use all functions defined in the previous exercises.
--
validate :: Integer -> Bool
validate n =
  checkDigit `mod` 10 == 0
    where checkDigit =  sumDigits $ doubleEveryOther $ toDigits n

prop_validate_ex1 = validate 4012888888881881 == True
prop_validate_ex2 = validate 4012888888881882 == False


-- 5. Tower of Hanoi
type Peg  = String
type Move = (Peg, Peg)
hanoi :: Int -> Peg -> Peg -> Peg -> [Move]
hanoi 0 _ _ _ = []
hanoi n a b c =
    -- move n-1 discs from a to c using b as temp
    hanoi (n-1) a c b
    ++
    -- move top disc from a to b
    [(a, b)]
    ++
    -- move n-1 discs from c to b using a as temp
    hanoi (n-1) c b a

-- Wacky boilerplate to make all tests run.
return []
-- run this to test all quickcheck properties
runTests = $quickCheckAll
