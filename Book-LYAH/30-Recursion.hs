-- http://learnyouahaskell.com/recursion#hello-recursion
--

{-
Recursion is actually a way of defining functions in which the function is applied inside its own definition.
Definitions in mathematics are often given recursively.

Having an element or two in a recursion definition defined non-recursively is also called the edge condition
and is important if you want your recursive function to terminate.

Recursion is important to Haskell because unlike imperative languages, you do computations in Haskell by
declaring what something is instead of declaring how you get it.
That's why there are no while loops or for loops in Haskell and instead we many times have to use recursion
to declare what something is.
-}

-- Quick Sort
-- Quicksort has become a sort of poster child for Haskell.
-- Therefore, let's implement it here, even though implementing quicksort in Haskell is considered really
-- cheesy because everyone does it to showcase how elegant Haskell is.
quicksort :: (Ord a) => [a] -> [a]
-- Edge Condition - A sorted empty list is an empty list
quicksort [] = []
-- a sorted list is a list that has all the values smaller than (or equal to)
-- the head of the list in front (and those values are sorted),
-- then comes the head of the list in the middle and then come all the values
-- that are bigger than the head
quicksort (x:xs) =  let lessThanX = [ y | y <- xs, y <= x]
                        sortedLessThanX = quicksort lessThanX
                        moreThanX = [ y | y <- xs, y > x]
                        sortedMoreThanX = quicksort moreThanX
                    in
                        sortedLessThanX ++ (x : sortedMoreThanX)


-- Thinking Recursively
{-
when trying to think of a recursive way to solve a problem,
    try to think of when a recursive solution doesn't apply
    and see if you can use that as an edge case,
    think about identities
       and
    think about whether you'll break apart the parameters of the function
    (for instance, lists are usually broken into a head and a tail via pattern matching)
    and on which part you'll use the recursive call.
-}
