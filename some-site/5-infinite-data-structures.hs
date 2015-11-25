import Test.QuickCheck

{-
constructors are being a special kind of function - they can be used in pattern-matching. They are non-strict too
-}

-- infinite ones
ones = 1 : ones

-- infinite list of numbers
numsFrom n = n : numsFrom (n+1)

squares = map (^2) (numsFrom 0)

prop_squares_lazy = (take 5 squares) == [0,1,4,9,16]

-- fibonacci as an infinite sequence
fib		= 1 : 1 : [ (a+b) | (a,b) <- zip fib (tail fib) ]