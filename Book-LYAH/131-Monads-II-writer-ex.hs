{-
 - https://github.com/noelmarkham/learn-you-a-haskell-exercises/blob/master/13-for-a-few-monads-more.hs
 -
 - We are going to write a binary search algorithm, which returns a Writer, so we can record the
 - intermediate steps.
 -
 - Note, There appear to have been some changes to ghc since Learn You a Haskell was written. There
 - is no data constructor exposed for Writer, like described in the chapter. You cannot do things
 - like: logNumber x = Writer (x, ["Got number: " ++ show x]). However there is a 'writer' function,
 - which works in the same way as that constructor would.
 - That is, you could do: logNumber x = writer (x, ["Got number: " ++ show x]).
 - For more information,
 -  see http://stackoverflow.com/questions/11684321/how-to-play-with-control-monad-writer-in-haskell
 -
 - That said, it is possible to create the Writers necessary for this exercise using only return and tell.
 -
 - It may help to write a function first, "describe", which returns a description of the comparison
 - at each step. This function would return a single element list of type String (remember, lists are
 - monoids). Such a function could run as follows:
 -
 - ghci> runWriter $ binarySearch describe 5 [1..10]
 - (True,["5 is less than 6","5 is greater than 3","5 is greater than 4","5 is equal to 5"])
 -
 - Or, perhaps a little clearer:
 - ghci> mapM_ putStrLn $ snd $ runWriter $ binarySearch describe 5 [1..10]
 - 5 is less than 6
 - 5 is greater than 3
 - 5 is greater than 4
 - 5 is equal to 5
 -}

import Control.Monad.Writer

comparisionAsString GT = " is greater than "
comparisionAsString LT = " is less than "
comparisionAsString EQ = " is equal to "

describeComparision :: (Show a, Eq a, Ord a) => a -> a -> [String]
describeComparision x y =
    let
        compStr = comparisionAsString $ compare x y
    in
        [(show x) ++ compStr ++ (show y)]

emptyList :: a -> [String]
emptyList a = ["Empty List - cannot find a"]

bs :: (Eq a, Ord a, Show a) => a -> [a] -> Writer [String] Bool
bs _ []     = do
                tell ["Empty list - cant find"]
                return False

bs a (x:xs)
    | x == a    = do
                    tell $ describeComparision x a
                    return True
    | otherwise = do
                    tell $ describeComparision x a
                    bs a xs

{-
 - The signature of our binary search is as follows:
 - binarySearch :: (Show a, Ord a, Eq a, Monoid b) => (a -> a -> b) -> a -> [a] -> Writer b Bool
 -
 - A breakdown of the function parameters:
 -   - (a -> a -> b) : A function which is called on each comparison, passing in the search value,
 -                     and the value to compare in the list. It returns a monoid value, which is
 -                     used in the Writer
 -   - a : The value we are hunting for
 -   - [a] : The list we are searching for the value, assumed to be ordered low to high
 -   - Writer b Bool : The return value of the Writer will be a monoid, constructed on each
 -                     (recursive) search step, and a Bool value, whether we found the value
 -                     in our list
 -
 - The pattern for the gcd' function in the chapter is helpful. That is: pattern match, guard and
 - using tell and return in the do notation.
 -}
binarySearch :: (Show a, Ord a, Eq a, Monoid b) => (a -> a -> b) -> a -> [a] -> Writer b Bool
binarySearch logFn _ []     = error "Empty list - cant find"
binarySearch logFn a (x:xs)
    | x == a    = do
                    tell $ logFn x a
                    return True
    | otherwise = do
                    tell $ logFn x a
                    binarySearch logFn a xs
{-
 Investigate what other functions instead of describe can be passed to the binary search.

 Is it possible to provide a function, so that when we do the binary search we can return
 a count of how many comparisons the algorithm took?
 -}
countComparisions :: (Show a, Eq a, Ord a) => a -> a -> (Sum Int)
countComparisions x y = 1

test_describe_binary_search = mapM_ putStrLn . snd . runWriter $ binarySearch describeComparision  5 [1..]
test_count_binary_search = putStrLn . show . getSum . snd . runWriter $ binarySearch countComparisions  5 [1..]