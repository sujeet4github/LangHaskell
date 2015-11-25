-- http://learnyouahaskell.com/starting-out#tuples
--

{-
Use tuples when you know in advance how many components some piece of data should have.
Tuples are much more rigid because each different size of tuple is its own type,
so you can't write a general function to append an element to a tuple —
  you'd have to write a function for appending to a pair,
      one function for appending to a triple,
      one function for appending to a 4-tuple, etc.

-}

-- these functions operate only on pairs. They won't work on triples, 4-tuples, 5-tuples, etc
tupleOps_first = fst ("Wow", False)
tupleOps_second = snd ("Wow", False)


tuples_Zip =    let index = [1..]
                    fruits = ["apple", "orange", "cherry", "mango"]
                in
                    zip index fruits


-- which right triangle that has integers for all sides
--  and all sides equal to or smaller than 10
--  has a perimeter of 24?
-- First, let's try generating all triangles with sides equal to or smaller than 10:
triangesSidesSmallerThan11 = [(x,y,z) | x <- [1..10], y <- [1..10], z <- [1..10] ]
-- We'll also modify this function by taking into consideration that side b isn't larger than the hypothenuse
-- and that side a isn't larger than side b
triangesSidesSmallerThan11AndRightTrianges = [(a,b,h) | h <- [1..10], b <- [1..h], a <- [1..b], (a*a) + (b*b) == (h*h) ]
triangesSidesSmallerThan11AndRightTriangesAndPerimiterOf24 = [(a,b,h) | h <- [1..10], b <- [1..h], a <- [1..b], (a*a) + (b*b) == (h*h), a+b+h == 24 ]