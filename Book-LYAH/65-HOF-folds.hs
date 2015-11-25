-- http://learnyouahaskell.com/higher-order-functions#folds
--

{-
Folds can be used to implement any function where you traverse
a list once, element by element, and then return something based
on that.

Whenever you want to traverse a list to return something,
chances are you want a fold.

That's why folds are, along with maps and filters, one of the
most useful types of functions in functional programming.

foldl
foldr
foldl1
foldr1

Scans are used to monitor the progression of a function that can be implemented as a fold.
scanl  - the final result will be in the last element of the resulting list
scanr  - the final result will be in the head element of the resulting list
scanl1
scanr1

Strict versions:
foldl'
foldr'
foldl1'
foldr1'

When using lazy folds on really big lists, you might often get a stack overflow error.
The culprit for that is that due to the lazy nature of the folds, the accumulator value
isn't actually updated as the folding happens.

What actually happens is that the accumulator kind of makes a promise that it will compute
its value when asked to actually produce the result (also called a thunk).

That happens for every intermediate accumulator and all those thunks overflow your stack.

The strict folds aren't lazy buggers and actually compute the intermediate values as they
go along instead of filling up your stack with thunks.

So if you ever get stack overflow errors when doing lazy folds, try switching to their strict versions.

-}

-- left fold - foldl

-- implement elem (checks whether a value is part of a list) with a left fold
elem' :: (Eq a, Foldable t) => a -> t a -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

-- implement sum as a left fold
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0


-- right fold - foldr
-- we usually use right folds when we're building up new lists from a list
-- because ++ operation is much more expensive than :

{-
One big difference is that right folds work on infinite lists,
whereas left ones don't!

To put it plainly, if you take an infinite list at some point
and you fold it up from the right, you'll eventually reach the
beginning of the list.

However, if you take an infinite list at a point and you try
to fold it up from the left, you'll never reach an end!

-}

-- How many elements does it take for the sum of the roots of all natural numbers to exceed 1000?
using_scanl1_ex1 = 1 + length resultList
                    where
                        infiniteSqRoots =  map sqrt [1..]
                        scanResult = scanl1 (+) infiniteSqRoots
                        -- use takeWhile rather than filter when working with infinite lists
                        resultList = takeWhile (< 1000) scanResult