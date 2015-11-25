-- http://learnyouahaskell.com/modules#data-list
--

import Data.List as DL
{-
Prelude module exports some functions (map, filter,...) from Data.List for convenience.
You don't have to import Data.List via a qualified import because it doesn't clash with
any Prelude names except for those that Prelude already steals from Data.List.

intersperse takes an element and a list and then puts that element in between each pair
    of elements in the list.

intercalate takes a list of lists and a list. It then inserts that list in between all
    those lists and then flattens the result

transpose transposes a list of lists. If you look at a list of lists as a 2D matrix,
    the columns become the rows and vice versa
-}

polynomialAdd :: (Num a) => [a]
polynomialAdd = let e1 = [  0, 3, 5,    9 ] -- 3x^2 + 5x + 9
                    e2 = [ 10, 0, 0,    9 ] -- 10x^3 + 9
                    e3 = [  8, 5, 1, (-1) ] -- 8x^3 + 5x^2 + x - 1
                in
                    map sum $ DL.transpose [e1, e2, e3]

{-

concat flattens a list of lists into just a list of elements.

concatMap is the same as first mapping a function to a list and then concatenating the list with concat.

and     takes a list of boolean values and returns True only if all the values in the list are True.
or      is like and, only it returns True if any of the boolean values in a list is True.
any,all takes a predicate and then check if any or all the elements in a list satisfy the predicate,
        respectively. Usually we use these two functions instead of mapping over a list and then doing
        and or or.

-}

testing_And = and $ map (>4) [5,6,7,8]

{-

iterate takes a function and a starting value.
    It applies the function to the starting value, then it applies that function to the result,
    then it applies the function to that result again, etc.
    It returns all the results in the form of an infinite list.

splitAt takes a number and a list.
    It then splits the list at that many elements, returning the resulting two lists in a tuple

takeWhile is a really useful little function.
    It takes elements from a list while the predicate holds and then when an element is encountered
    that doesn't satisfy the predicate, it's cut off.

dropWhile is similar, only it drops all the elements while the predicate is true.
    Once predicate equates to False, it returns the rest of the list.

span is kind of like takeWhile, only it returns a pair of lists.
    The first list contains everything the resulting list from takeWhile would contain if it were
    called with the same predicate and the same list.
    The second list contains the part of the list that would have been dropped.

break breaks it when the predicate is first true.
    Doing break p is the equivalent of doing span (not . p)

sort simply sorts a list. The type of the elements in the list has to be part of the Ord typeclass,
    because if the elements of a list can't be put in some kind of order, then the list can't be sorted.

group takes a list and groups adjacent elements into sublists if they are equal.
    If we sort a list before grouping it, we can find out how many times each element appears in the list.

inits and tails are like init and tail, only they recursively apply that to a list until there's nothing left.

isInfixOf searches for a sublist within a list and returns True if the sublist we're looking for
    is somewhere inside the target list.

isPrefixOf and isSuffixOf search for a sublist at the beginning and at the end of a list, respectively.

elem and notElem check if an element is or isn't inside a list.

partition takes a list and a predicate and returns a pair of lists.
    The first list in the result contains all the elements that satisfy the predicate,
    the second contains all the ones that don't.

find takes a list and a predicate and returns the first element that satisfies the predicate.
    But it returns that element wrapped in a Maybe value.

elemIndex is kind of like elem, only it doesn't return a boolean value. It maybe returns the index
    of the element we're looking for. If that element isn't in our list, it returns a Nothing.

elemIndices is like elemIndex, only it returns a list of indices, in case the element we're looking
    for crops up in our list several times.

findIndex is like find, but it maybe returns the index of the first element that satisfies the predicate.

findIndices returns the indices of all elements that satisfy the predicate in the form of a list.

zip, zipWith. - they zip together two lists, either in a tuple or with a binary function
zip3, zip4, etc. and zipWith3, zipWith4, etc  for more than 2 lists, upto 7 lists.

lines is a useful function when dealing with files or input from somewhere.
    It takes a string and returns every line of that string in a separate list.

unlines is the inverse function of lines. It takes a list of strings and joins them together
    using a '\n'.

words and unwords are for splitting a line of text into words or joining a list of words into a text.

nub. It takes a list and weeds out the duplicate elements, returning a list whose every element is unique.
    "nub" means a small lump or essential part of something

delete takes an element and a list and deletes the first occurence of that element in the list.

\\ is the list difference function. It acts like a set difference, basically.
    For every element in the right-hand list, it removes a matching element in the left one.

union also acts like a function on sets. duplicates are removed from the second list!
intersect works like set intersection. It returns only the elements that are found in both lists.

insert takes an element and a list of elements that can be sorted and inserts it into the last
    position where it's still less than or equal to the next element.

length, take, drop, splitAt, !! and replicate   -> take an Int as parameter or return
genericLength, genericTake, genericDrop, genericSplitAt, genericIndex and genericReplicate
    are generic equivalents.

nub, delete, union, intersect and group use == to test for equality.
nubBy, deleteBy, unionBy, intersectBy and groupBy takes an equality function.
    group is the same as groupBy (==).

sortBy, insertBy, maximumBy and minimumBy are generic equivalents of sort, insert, maximum and minimum
    take a function that determine if one element is greater, smaller or equal to the other


-}
