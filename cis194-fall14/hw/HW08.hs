module HW08 where

import Control.Monad
import Data.Maybe
import Text.Read
import Data.List

{- Ex 01
Write a function that detects whether or not a string has
a certain format. The required format is as follows:
1. The string starts with a digit.
2. Say the value of this digit is n. The string next contains n as.
3. After the n as, either the string ends or the sequence repeats,
starting with a (perhaps different) digit.
Here are some strings that match this format and some that don’t:
    stringFitsFormat "3aaa2aa" == True
    stringFitsFormat "3aaa2a" == False
    stringFitsFormat "9aaaaaaaaa" == True
    stringFitsFormat "10aaaaaaaaaa" == False
    stringFitsFormat "0" == True
    stringFitsFormat "1" == False
    stringFitsFormat "001a" == True
    stringFitsFormat "100a" == False
    stringFitsFormat "2aa2aa" == True
    stringFitsFormat "2bb2bb" == False
-}

stringFitsFormat :: String -> Bool
stringFitsFormat = isJust . go
    where
        go :: String -> Maybe String
        -- go evaluates to ‘Just ""‘ on success, or Nothing otherwise
        go s = if s == "" then Nothing else checkPattern s

checkPattern :: String -> Maybe String
checkPattern [] = Just ""
checkPattern xs = do
    let firstCharStr = take 1 xs
    let ys = tail xs
    countOfa <- readMaybe firstCharStr
    let stringOfa = replicate countOfa 'a'
    zs <- stripPrefix stringOfa ys
    if "" == zs
    then
        return ""
    else
        checkPattern zs

-- Ex 2
specialNumbers :: [Int]
specialNumbers = [x | x <- [1..100], (rem x 5) == 0, (rem x 7) /= 0]
specialNumbers2 :: [Int]
specialNumbers2 = do
    x <- [1..100]
    guard((rem x 5) == 0)
    guard((rem x 7) /= 0)
    return x
