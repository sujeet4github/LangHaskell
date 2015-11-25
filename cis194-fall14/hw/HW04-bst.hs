-- http://www.seas.upenn.edu/~cis194/fall14/hw/04-poly.pdf
module HW04bst where

{-
A binary search tree is a recursive data stucture frequently used to
store a set—that is, a chunk of data that is easily (and efficiently)
searched and added to.
-}
import BST
import Data.Char
import Data.List


-- Exercise 13
-- Write the insertion method for a binary search tree
insertBST :: (a -> a -> Ordering) -> a -> BST a -> BST a
{-
This function is actually quite simple—thinking through the answers
to the following questions will essentially write the function for you:
1. What should you do when inserting into an empty tree (that is, a Leaf)?
2. What should you do when inserting x into a non-empty tree
    whose root node has a value greater than x?
3. What should you do when inserting x into a non-empty tree
    whose root node has a value less than x?

It is interesting to note that, because of parametric polymorphism,
every call of insertBST must be accompanied by a comparison operation.
Otherwise, there’s no way to know how to compare elements.
We’ll see a mechanism—called type classes—that will make this less
burdensome.
-}
insertBST _ x Leaf = Node Leaf x Leaf
insertBST f x (Node l y r) = case f x y of
    EQ  ->  (Node l y r)
    LT  ->  (Node (insertBST f x l) y r)
    GT  ->  (Node l y (insertBST f x r))


{-
An effective Haskell programmer must know how to use the standard
libraries. The exercises below will require you to read through
the documentation of Data.List, Data.Maybe, and Data.Char to write
succinct solutions. Each of these functions has a simple, one-liner
answer!
-}
-- Exercise 14
-- Check to see if a list of strings contains only capitalized words:
allCaps :: [String] -> Bool
allCaps [] = True
allCaps xs = all isCapitalizedWord xs
    where
        isCapitalizedWord []    = False
        isCapitalizedWord [c]   = isAsciiUpper c
        isCapitalizedWord (c:cs)= isAsciiUpper c && (all isAsciiLower cs)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (_:xs) = Just xs


stringIsCapitalized :: String -> Bool
-- 1. safeHead may fail and return Nothing, in that case - return False
--    otherwise return result of isAsciiUpper on the value held in Just
-- 2. safeTail may fail and return Nothing if there is tail (single char)
--    string. return True in that case
--    otherwise return result of isAsciiLower on all of String held in Just
stringIsCapitalized cs = maybe False isAsciiUpper (safeHead cs)
        && maybe True (all isAsciiLower) (safeTail cs)

allCaps2 :: [String] -> Bool
allCaps2 cs =   maybe True stringIsCapitalized (safeHead cs)
                && maybe True allCaps2 (safeTail cs)

-- making a one liner by making the stringIsCapitalized a lambda
--
allCaps3 :: [String] -> Bool
allCaps3 cs =   maybe True (\x ->
                                maybe False isAsciiUpper (safeHead x)
                                &&
                                maybe True (all isAsciiLower) (safeTail x)
                                ) (safeHead cs)
                && maybe True allCaps3 (safeTail cs)

allCapsFunctionList = [allCaps, allCaps2, allCaps3]
t1 = testMultipleBooleanFunctions allCapsFunctionList goodStringList True
    where
        goodStringList = [["Hi", "There"], []]

t2 = testMultipleBooleanFunctions allCapsFunctionList failingStringList False
    where
        failingStringList = [["", "Blah"], ["Hi","there"]]

lengthAtleast :: Int -> [a] -> Bool
lengthAtleast n []  = n >= 0
lengthAtleast 0 _   = False
lengthAtleast n (x:xs) = lengthAtleast (n-1) xs

testMultipleBooleanFunctions :: [a -> Bool] -> [a] -> Bool -> Bool
testMultipleBooleanFunctions fns args expectedBool =
    let
        countFns    = length fns
        countArgs   = length args
        countActuals= countFns * countArgs
    in
        testMultipleFunctions fns args (replicate countActuals expectedBool) True

testMultipleFunctions :: (Eq b) => [a -> b] -> [a] -> [b] -> Bool -> Bool
testMultipleFunctions fns args expectations expectedBool =
    let
        countFns    = length fns
        countArgs   = length args
        countActuals= countFns * countArgs
    in
        if (lengthAtleast countActuals expectations)
        then all (== expectedBool) $ applyManyFunctionsOnManyArgsAndCompareWithExpected fns args expectations
        else error "Mismatch in count of expected and actual results"

zipApplyManyFunctionsOnManyArgs :: [a -> b] -> [a] -> [b]
zipApplyManyFunctionsOnManyArgs fns args =
    if (length fns == length args)
    then zipWith (\f x -> f x) fns args
    else error "Mismatch in count of expected and actual results"

applyManyFunctionsOnManyArgs fns args =
    concat $ map (\f -> applyFn f args) fns
    where applyFn f args = map f args

applyManyFunctionsOnManyArgsAndCompareWithExpected :: (Eq b) => [a -> b] -> [a] -> [b] -> [Bool]
applyManyFunctionsOnManyArgsAndCompareWithExpected fns args results =
    zipApplyManyFunctionsOnManyArgs (map (==) results) (applyManyFunctionsOnManyArgs fns args)

-- Exercise 15
-- Drop the trailing whitespace from a string:
dropTrailingWhitespace :: String -> String
dropTrailingWhitespace = dropWhileEnd isSpace

-- Exercise 16
-- Get the first letter (if it exists) of a list of strings:
firstLetters :: [String] -> [Char]
firstLetters ss = concat $ map stringWithFirstChar ss
    where
        stringWithFirstChar s = maybe [] (\x -> [x]) $ safeHead s

testFL = testMultipleFunctions firstLetterFunctions arguments expectedResults True
    where
        firstLetterFunctions = [firstLetters]
        arguments = [ ["foo", "bar"] , ["alpha",""] , [] , ["",""] ]
        expectedResults = [ ['f','b'] , ['a'] , [] , []]


-- Exercise 17
-- Render a proper bracketed list given a list of strings:
asList :: [String] -> String
asList xs = intercalate (intercalate "," xs) [ "[" , "]" ]

testAsList = testMultipleFunctions asListFunctions arguments expectedResults True
    where
        asListFunctions = [asList]
        arguments = [ ["alpha","beta","gamma"] , [] , ["lonely"] ]
        expectedResults = [ "[alpha,beta,gamma]" , "[]" , "[lonely]" ]
