-- http://learnyouahaskell.com/higher-order-functions#maps-and-filters
--

{-
Mapping and filtering is the bread and butter of every functional programmer's toolbox.
It doesn't matter if you do it with the map and filter functions or list comprehensions.
Recall how we solved the problem of finding right triangles with a certain circumference. (list comprehensions)
With imperative programming, we would have solved it by nesting three loops
    and then testing if the current combination satisfies a right triangle
    and if it has the right perimeter.
 If that's the case, we would have printed it out to the screen or something.

In functional programming, that pattern is achieved with mapping and filtering.
You make a function that takes a value and produces some result.
We map that function over a list of values and then we filter the resulting list out for the results
that satisfy our search.

Thanks to Haskell's laziness, even if you map something over a list several times and filter it several times,
it will only pass over the list once.

-}

-- map takes a function and a list and applies that function to every element in the list,
-- producing a new list.
-- map :: (a -> b) -> [a] -> [b]

ex_map_1 = map (++ "!") ["BIFF", "BANG", "POW"]
ex_map_2 = map (replicate 3) [3..6]
ex_map_3 = map (map (^2)) [[1,2],[3,4,5,6],[7,8]]

-- map (+3) [1,5,3,1,6] is the same as writing [x+3 | x <- [1,5,3,1,6]].
-- However, using map is much more readable for cases where you only apply some function
-- to the elements of a list, especially once you're dealing with maps of maps and then
-- the whole thing with a lot of brackets can get a bit messy.
equiv_map_list_compr = lhs == rhs
                 where
                    lhs = map (+3) [1,5,3,1,6]
                    rhs = [x+3 | x <- [1,5,3,1,6]]


-- filter :: (a -> Bool) -> [a] -> [a]

ex_filter_1 = let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
ex_filter_2 = filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"

-- All of this could also be achived with list comprehensions by the use of predicates.

-- Quicksort using filter instead of list comprehension
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =  let lessThanX = filter (<= x) xs
                        sortedLessThanX = quicksort lessThanX
                        moreThanX = filter (> x) xs
                        sortedMoreThanX = quicksort moreThanX
                    in
                        sortedLessThanX ++ (x : sortedMoreThanX)
