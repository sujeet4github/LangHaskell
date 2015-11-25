{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module Golf where

import Safe
import Test.QuickCheck
import Test.QuickCheck.All()


-- Exercise 1 Hopscotch
-- The output of skips is a list of lists. The first list in the output should
-- be the same as the input list. The second list in the output should
-- contain every second elemento from the input list. . . and the nth list in
-- the output should contain every nth element from the input list.
skips :: [a] -> [[a]]
skips xs = map (\n -> getNElements n xs) [1..len]
            where len = length xs

getNElements :: Int -> [a] -> [a]
getNElements n xs = if n > (length xs)
                      then []
                      else (xs `at` (n-1)) : (getNElements n (drop n xs))

-- version of getNEElements w/o recursion
skip :: Int -> [a] -> [a]
-- skip n = map snd . filter (\x -> mod (fst x) n == 0) . zip [1..]
skip n ns = returnList
            where
              valueAndIndexTuples = zip [1..] ns
              filteredValueIndexTuples = filter (\x -> mod (fst x) n == 0) valueAndIndexTuples
              returnList = map snd filteredValueIndexTuples

prop_st_getNElements_for1 :: (Eq a) => [a] -> Bool
prop_st_getNElements_for1 xs = (getNElements 1 xs) == xs

prop_st_getNElements_ex2 :: Bool
prop_st_getNElements_ex2 = getNElements 2 "hello!" == "el!"
prop_st_getNElements_ex3 :: Bool
prop_st_getNElements_ex3 = getNElements 3 "hello!" == "l!"
prop_st_getNElements_ex4 :: Bool
prop_st_getNElements_ex4 = getNElements 4 "hello!" == "l"
prop_st_getNElements_ex5 :: Bool
prop_st_getNElements_ex5 = getNElements 5 "hello!" == "o"
prop_st_getNElements_ex6 :: Bool
prop_st_getNElements_ex6 = getNElements 6 "hello!" == "!"

prop_st_skips :: (Eq a) => [a] -> Bool
prop_st_skips xs = lengthMatches && firstElementMatches && allElementsMatch
        where
            runResult = skips xs
            lengthMatches = (length xs) == (length runResult)
            firstElementMatches = if (length xs == 0)
                                    then
                                      lengthMatches
                                    else
                                      xs == (runResult `at` 0)
            flags = map (\n -> (getNElements n xs) == (runResult `at` (n - 1))) [1..length xs]
            allElementsMatch = all (== True) flags

prop_st_skips_ex1 :: Bool
prop_st_skips_ex1 = skips "ABCD" == ["ABCD", "BD", "C", "D"]
prop_st_skips_ex2 :: Bool
prop_st_skips_ex2 = skips "hello!" == ["hello!", "el!", "l!", "l", "o", "!"]
prop_st_skips_ex3 :: Bool
prop_st_skips_ex3 = skips [1] == [[1]]
prop_st_skips_ex4 :: Bool
prop_st_skips_ex4 = skips [True,False] == [[True,False], [False]]


-- Exercise 2 Local maxima
-- A local maximum of a list is an element of the list which is strictly
-- greater than both the elements immediately before and after it. For
-- example, in the list [2,3,4,1,5], the only local maximum is 4, since
-- it is greater than the elements immediately before and after it (3 and
-- 1). 5 is not a local maximum since there is no element that comes
-- after it.
localMaxima :: [Integer] -> [Integer]
localMaxima ns = returnList
                  where
                    groupsOfThree = map (\n -> (getGroupOfThree n ns)) [1..(length ns - 2)]
                    groupsWithLocalMaxima = filter (\n2s -> isLocalMaxima n2s) groupsOfThree
                    returnList = map (\n2s -> n2s `at` 1) groupsWithLocalMaxima

isLocalMaxima :: [Integer] -> Bool
isLocalMaxima [l,m,r] = m > l && m > r
isLocalMaxima _       = False

prop_isLocalMaxima :: [Integer] -> Bool
prop_isLocalMaxima ns@[lVal, mVal, rVal] = expectedResult == actualResult
                                              where
                                                actualResult = isLocalMaxima ns
                                                expectedResult = (mVal > lVal && mVal > rVal)
prop_isLocalMaxima ns = False == isLocalMaxima ns

-- simpler version
localMaxima' :: [Integer] -> [Integer]
localMaxima' (a:b:c:ns)
  | (b > a) && (b > c)  = b : localMaxima' (b:c:ns)
  | otherwise           = localMaxima' (b:c:ns)
localMaxima' _  = []

getGroupOfThree :: Int -> [Integer] -> [Integer]
getGroupOfThree n ns
  | n < 1                 = []
  | n >= (length ns - 1)  = []
  | otherwise             = [elemOne,elemTwo,elemThree]
                            where
                              elemOne = ns `at` (n-1)
                              elemTwo = ns `at` n
                              elemThree = ns `at` (n+1)

prop_getGroupsOfThree :: Int -> [Integer] -> Bool
prop_getGroupsOfThree n ns
  | (length ns) < 3       = null $ getGroupOfThree n ns
  | n < 1                 = null $ getGroupOfThree n ns
  | n >= (length ns) - 1  = null $ getGroupOfThree n ns
  | otherwise             = actualResult == expectedResult
                            where
                                actualResult = getGroupOfThree n ns
                                expectedResult = [ns !! (n-1), ns !! n, ns !! (n+1)]


prop_st_localMaxima_ex1 :: Bool
prop_st_localMaxima_ex1 = localMaxima [1,2,3,4,5] == []
prop_st_localMaxima_ex2 :: Bool
prop_st_localMaxima_ex2 = localMaxima [2,3,4,1,5] == [4]
prop_st_localMaxima_ex3 :: Bool
prop_st_localMaxima_ex3 = localMaxima [2,9,5,6,1] == [9,6]

-- Exercise 3 histogram
-- takes as input a list of Integers between 0 and 9 (inclusive),
-- and outputs a vertical histogram showing how many of each number
-- were in the input list. You may assume that the input list does not
-- contain any numbers less than zero or greater than 9 (that is, it does
-- not matter what your function does if the input does contain such
-- numbers). Your output must exactly match the output shown in the
-- examples below
histogram :: [Integer] -> String
histogram ns = concat (eachHistogram) ++ histogramLegend
              where eachHistogram = map (\n -> (histogramPoint n) ++ "\n") ns

histogramPoint :: Integer -> String
histogramPoint n
  | n < 0 = "<Error-should be between 0 and 9>"
  | n > 9 = "<Error-should be between 0 and 9>"
  | otherwise = map (\index -> if (n == index) then '*' else ' ') [0..9]

histogramLegend :: String
histogramLegend = "==========\n0123456789\n"


-- Wacky boilerplate to make all tests run.
return []
-- run this to test all quickcheck properties
runTests :: IO Bool
runTests = $quickCheckAll
