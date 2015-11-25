{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TemplateHaskell #-}
import Test.QuickCheck
import Test.QuickCheck.All()

-- Higher-order programming and type inference
-- http://www.seas.upenn.edu/~cis194/spring13/lectures/04-higher-order.html

-- Suggested reading:
-- Learn You a Haskell for Great Good* chapter "Higher-Order Functions"
-- (Chapter 5 in the printed book;
--  [Chapter 6 online](http://learnyouahaskell.com/higher-order-functions))

-- The awesomeness of partial application...

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

return16 :: Int
return16 = (applyTwice (+ 3) 10)
prop_applyTwice_ex1 :: Bool
prop_applyTwice_ex1 = 16 == return16
prop_applyTwice_ex2 :: Bool
prop_applyTwice_ex2 = "HEY HAHA HAHA" == (applyTwice (++ " HAHA") "HEY")
prop_applyTwice_ex3 :: Bool
prop_applyTwice_ex3 = "HAHA HAHA HEY" == (applyTwice ("HAHA " ++) "HEY")
return331 :: [Int]
return331 = (applyTwice (3:) [1])
prop_applyTwice_ex4 :: Bool
prop_applyTwice_ex4 = [3,3,1] == return331

tt :: [Double]
tt = map ($ 3) [(4+), (10*), (^ (2::Integer)), sqrt]

-- The awesomeness of higher order functions...

-- It takes a function and two lists as parameters and then joins the two lists
-- by applying the function between corresponding elements.
--
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- if any of the lists are empty, return is empty
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
-- The first parameter is a function that takes two things and produces a
-- third thing. They don't have to be of the same type, but they can. The
-- second and third parameter are lists. The result is also a list.
--  The first has to be a list of a's, because the joining function
--  takes a's as its first argument. The second has to be a list of b's,
--  because the second parameter of the joining function is of type b.
-- The result is a list of c's. If the type declaration of a function says
-- it accepts an a -> b -> c function as a parameter, it will also accept
-- an a -> a -> a function, but not the other way around! Remember that
-- when you're making functions, especially higher order ones, and you're
-- unsure of the type, you can just try omitting the type declaration and
--  then checking what Haskell infers it to be by using :t.
prop_zipW_ex1 :: Bool
prop_zipW_ex1 = let x = zipWith' (+) [4,2,5,6] [2,6,2,3] :: [Int]
                  in x == [6,8,7,9]
prop_zipW_ex2 :: Bool
prop_zipW_ex2 = let x = zipWith' (max) [4,2,5,6] [2,6,2,3] :: [Int]
                  in x == [7,3,2,5]
--
prop_zipW_ex3 :: Bool
prop_zipW_ex3 = let x = zipWith' (++) ["foo ", "bar ", "baz "] ["fighters", "hoppers", "aldrin"]
                  in x == ["foo fighters","bar hoppers","baz aldrin"]
--
prop_zipW_ex4 :: Bool
prop_zipW_ex4 = let x = zipWith' (*) (replicate 5 2) [1..]  :: [Int]
                  in x == [2,4,6,8,10]
--
-- inner function is applied to each pair of list
-- rewriting this:
--  -- applying defn-3 of zipWith'
--  zipWith' (*) [1,2,3] [3,2,2] : zipWith' (zipWith' (*)) [[3,5,6],[2,3,4]] [[3,4,5],[5,4,3]]
--  -- Now, zipWith' (*) [1,2,3] [3,2,2] = [3,4,6]
--  [3,4,6] : zipWith' (zipWith' (*)) [[3,5,6],[2,3,4]] [[3,4,5],[5,4,3]]
--  -- applying defn-3 of zipWith'
--  [3,4,6] : (zipWith' (*) [3,5,6] [3,4,5] : zipWith' (zipWith' (*)) [[2,3,4]] [[5,4,3]])
--  -- Now, zipWith' (*) [3,5,6] [3,4,5] = [9,20,30]
--  [3,4,6] : ([9,20,30] : zipWith' (zipWith' (*)) [[2,3,4]] [[5,4,3]])
--  -- applying defn-3 of zipWith'
--  [3,4,6] : ([9,20,30] : (zipWith' (*) [2,3,4] [5,4,3] : zipWith' (zipWith' (*)) [] []))
--  -- Now, zipWith' (*) [2,3,4] [5,4,3] = [10,12,12]
--  [3,4,6] : ([9,20,30] : ([10,12,12] : zipWith' (zipWith' (*)) [] []))
--  -- applying defn-1 of zipWith'
--  [3,4,6] : ([9,20,30] : ([10,12,12] : []))

prop_zipW_ex5 :: Bool
prop_zipW_ex5 = let x = zipWith' (zipWith' (*)) [[1,2,3],[3,5,6],[2,3,4]] [[3,2,2],[3,4,5],[5,4,3]]  :: [[Int]]
                  in x == [[3,4,6],[9,20,30],[10,12,12]]

-- takes a function and returns a function that is like our original
-- function, only the first two arguments are flipped.
-- Because functions are curried by default, the second pair of parentheses is really unnecessary
-- flip' :: (a -> b -> c) -> (b -> a -> c)
flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x
-- Reading the type declaration, we say that it takes a function that
-- takes an a and a b and returns a function that takes a b and an a.

prop_flip_ex1 :: Bool
prop_flip_ex1 = let x = flip' zip [1,2,3,4,5] "hello"  :: [(Char, Int)]
                  in x == [('h',1),('e',2),('l',3),('l',4),('o',5)]
prop_flip_zipWith_ex :: Bool
prop_flip_zipWith_ex = let x = zipWith (flip' div) [2,2..] [10,8,6,4,2] :: [Int]
                        in x == [5,4,3,2,1]


-- map takes a function and a list and applies that function to every element
-- in the list, producing a new list.
--
-- ghci> map (+3) [1,5,3,1,6]
-- [4,8,6,4,9]
-- ghci> map (++ "!") ["BIFF", "BANG", "POW"]
-- ["BIFF!","BANG!","POW!"]
-- ghci> map (replicate 3) [3..6]
-- [[3,3,3],[4,4,4],[5,5,5],[6,6,6]]
-- ghci> map (map (^2)) [[1,2],[3,4,5,6],[7,8]]
-- [[1,4],[9,16,25,36],[49,64]]
-- ghci> map fst [(1,2),(3,5),(6,3),(2,6),(2,5)]
-- [1,3,6,2,2]

-- filter is a function that takes a predicate (a predicate is a function that
--  tells whether something is true or not, so in our case, a function that
--  returns a boolean value) and a list and then returns the list of elements
--  that satisfy the predicate.
--
-- ghci> filter (>3) [1,5,3,2,1,6,4,3,2,1]
-- [5,6,4]
-- ghci> filter (==3) [1,2,3,4,5]
-- [3]
-- ghci> filter even [1..10]
-- [2,4,6,8,10]
-- ghci> let notNull x = not (null x) in filter notNull [[1,2,3],[],[3,4,5],[2,2],[],[],[]]
-- [[1,2,3],[3,4,5],[2,2]]
-- ghci> filter (`elem` ['a'..'z']) "u LaUgH aT mE BeCaUsE I aM diFfeRent"
-- "uagameasadifeent"
--

-- both map/filter effects can be done using list comprehensions and predicates
-- There's no set rule for when to use map and filter versus using list
-- comprehension, you just have to decide what's more readable depending on the
-- code and the context.

-- find the largest number under 100,000 that's divisible by 3829. To do that,
--  we'll just filter a set of possibilities in which we know the solution lies.
largestDivisible3829_1 :: (Integral a) => a
largestDivisible3829_1 =
  let
    filterFn x = x `mod` 3829 == 0
    filteredNumbers = filter filterFn [100000,99999..]
  in head filteredNumbers
largestDivisible3829_2 :: (Integral a) => a
largestDivisible3829_2 = head (filter predFn [100000,99999..])
            where predFn x = x `mod` 3829 == 0

-- find the sum of all odd squares that are smaller than 10,000.
sumResultUsingMap :: Integer
sumResultUsingMap = sum (takeWhile (<10000) (filter odd (map sq [1..])))
              where sq = (^ (2 :: Integer))
sumResultUsingLC :: Integer
sumResultUsingLC = sum (takeWhile (<10000) [sq n | n <- [1..], odd (sq n)])
              where sq = (^ (2 :: Integer))

-- For our next problem, we'll be dealing with Collatz sequences.
-- http://mathworld.wolfram.com/CollatzProblem.html
-- Collatz Conjecture:
-- Take any natural number n.
--  If n is even, divide it by 2 to get n / 2.
--  If n is odd, multiply it by 3 and add 1 to obtain 3n + 1.
--  Repeat the process (which has been called "Half Or Triple Plus One",
--    or HOTPO[6]) indefinitely.
--  The conjecture is that no matter what number you start with,
--  you will always eventually reach 1.
--  The property has also been called oneness.
--
chain :: (Integral a) => a -> [a]
-- Because the chains end at 1, that's the edge case.
chain 1 = [1]
chain n
  | even n  = n:chain (n `div` 2)
  | otherwise   = n:chain (n*3 + 1)
-- Now what we want to know is this:
--  for all starting numbers between 1 and 100,
--  how many chains have a length greater than 15?
numLongChains :: Int
numLongChains = length (filter isLong (map chain initialList))
                where
                  initialList = [1..100] :: [Int]
                  isLong xs = length xs > 15
-- Note: This function has a type of numLongChains :: Int
-- because length returns an Int instead of a Num a for historical reasons.
-- If we wanted to return a more general Num a, we could have used
-- fromIntegral on the resulting length.

--
-- Anonymous functions
-- Example
-- (\x y z -> [x,2*y,3*z]) 5 6 3
-- operator section
-- if ? is an operator,
--   then (?y) is equivalent to the function \x -> x ? y,
--    and (y?) is equivalent to \x -> y ? x.
-- In other words, using an operator section allows us to partially apply an operator
--  to one of its two arguments.
-- e.g.
-- (>100) 102
-- (100>) 102
-- map (*6) [1..5]

-- Function Composition
--
-- Prefer Function Composition to Anonymous Functions for readability
-- .
tt1 :: [Integer]
tt1 = map (negate . abs) [5,-3,-6,7,-3,2,-19,24]
prop_tt1 :: Bool
prop_tt1 = tt1 == [-5,-3,-6,-7,-3,-2,-19,-24]

tt2 :: [Integer]
tt2 = map (negate . sum . tail) [[1..5],[3..6],[1..7]]
prop_tt2 :: Bool
prop_tt2 = tt2 == [-14,-15,-27]

-- what about functions that take several parameters?
-- function composition with partially applied versons so that each function
-- takes just one argument
func_comp_with_multi_param_functions_ex1 :: Bool
func_comp_with_multi_param_functions_ex1 = lhs_multiParamOldWay == rhsWithComp
  where
      lhs_multiParamOldWay = sum (replicate 5 (max 6.7 8.9)) ::Double
      rhsWithComp = (sum . replicate 5 . max 6.7 $ 8.9)

-- sumResultUsingMap using function composition
-- replace the 3 closing parentheses with 3 . and 1 $
sumResultUsingFnCompositon :: Integer
sumResultUsingFnCompositon = sum . takeWhile (<10000) . filter odd . map sq $ [1..]
              where sq = (^ (2 :: Integer))
-- However, if there was a chance of someone else reading that code, I would have written it like this:
oddSquareSum :: Integer
oddSquareSum =
    let oddSquares = filter odd $ map (^2) [1..]
        belowLimit = takeWhile (<10000) oddSquares
    in  sum belowLimit

-- Currying and Partial application
-- f x y z = ...
--  is syntax sugar for
--  f = \x -> (\y -> (\z -> ...)).
--
-- function arrows associate to the right
-- Function application, in turn, is left-associative. That is, f 3 2 is really shorthand for (f 3) 2
--
-- This idea of representing multi-argument functions as one-argument functions
-- returning functions is known as currying
--
-- the standard library defines functions called curry and uncurry
-- uncurry (+) (2,3)


-- Pointfree / Point-free style
-- This style of coding in which we define a function without reference to its
-- arguments—in some sense saying what a function is rather than what it
-- does—is known as “point-free” style.
--
foobar' :: [Integer] -> Integer
foobar' = sum . map (\x -> (7*x + 2)) . filter (>3)
-- is more idiomatic than...
foobar :: [Integer] -> Integer
foobar []     = 0
foobar (x:xs)
  | x > 3     = (7*x + 2) + foobar xs
  | otherwise = foobar xs

-- recursion etc are delegated to library implementations

-- Folds
-- They're sort of like the map function, only they reduce the list to
-- some single value.
-- :t foldl
-- foldl :: Foldable t => (b -> a -> b) -> b -> t a -> b
--
-- foldl folds the list up from the LHS, the binary function is
-- applied between the accumulator value and the head of the list
-- That produces a new accumulator value and the binary function is
-- called with that value and the next element, etc.

-- writing sum of list as a fold
sum' :: (Num a) => [a] -> a
-- sum' xs = foldl (\acc -> (+ acc)) 0 xs
-- more succint than above
--  since The lambda function (\acc x -> acc + x)
--              is the same as partial application (+)
-- and We can omit the xs as the parameter because calling foldl (+) 0
-- will return a function that takes a list.
sum' = foldl (+) 0
-- sum' [3,5,2,1]
-- Walkthrough:
-- sum' [3,5,2,1]
-- = foldl (\acc -> (+ acc)) 0 [3,5,2,1]
-- = foldl (\acc -> (+ acc)) 3 [5,2,1]
-- = foldl (\acc -> (+ acc)) 8 [2,1]
-- = foldl (\acc -> (+ acc)) 10 [1]
-- = foldl (\acc -> (+ acc)) 11 []
-- = 11

-- writing length of list as Fold
length' :: [a] -> Int
length'   = foldr (\_ acc -> 1 + acc) 0
length'' :: [a] -> Int
length''  = foldr (\_ -> (1+)) 0
length''' :: [a] -> Int
length''' = foldr (const (1+)) 0

prop_len_check :: [a] -> Bool
prop_len_check arr = (length arr) == (length' arr)
                      && (length' arr) == (length'' arr)
                      && (length''' arr) == (length'' arr)

-- fold is already provided in the standard Prelude, under the name foldr
-- The function foldr is to fold the list with a right associative operator,
-- foldr f z [a,b,c] == a `f` (b `f` (c `f` z))
-- you can easily understand what the function does if you use the operator(+),
-- (The function has the same behavior as sum):
-- foldr (+) 0 [1,2,3,4,5] = 1+(2+(3+(4+(5+0))))
-- foldr (\_ n -> 1 + n) 0 [1,2,3,4,5] = 1+(1+(1+(1+(1+0))))

-- Another interpretation of foldr f z xs
-- z is the solution of the problem when the list is empty
-- f is a function that takes two arguments:
--   an element of the list,
--   and a partial solution:
--    the solution to your problem for the list of elements
--    that appear to the right of the element that's passed to f.
--   f then produces a solution that's "one element bigger."
--
-- foldl - folds from the left
-- foldl f z [a,b,c] == ((z `f` a) `f` b) `f` c
-- In general, however, you should use foldl' from Data.List instead,
-- which does the same thing as foldl but is more efficient.
-- Discussion about this in (http://haskell.org/haskellwiki/Foldr_Foldl_Foldl%27)
-- also in cis194-06.hs in sectio about Laziness
--

-- standard library functions using fold's
--
maximum' :: (Ord a) => [a] -> a
maximum' = foldr1 (\x acc -> if x > acc then x else acc)

reverse' :: [a] -> [a]
reverse' = foldl (\acc x -> x : acc) []
reverse'' :: [a] -> [a]
reverse'' = foldl (flip (:)) []

-- say we have a right fold and the binary function is f and the starting
--  value is z.
-- If we're right folding over the list [3,4,5,6],
--  we're essentially doing this:
-- f 3 (f 4 (f 5 (f 6 z))).
-- Similarly, doing a left fold over that list with g as the binary function
-- and z as the accumulator is the equivalent of
--  g (g (g (g z 3) 4) 5) 6.
--
-- If we use flip (:) as the binary function and [] as the accumulator
--  (so we're reversing the list), then that's the equivalent of
-- flip (:) (flip (:) (flip (:) (flip (:) [] 3) 4) 5) 6
-- which evaluates to [6,5,4,3]

product' :: (Num a) => [a] -> a
product' = foldr1 (*)

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x acc -> if p x then x : acc else acc) []

-- The foldl1 and foldr1 functions work much like foldl and foldr,
-- only you don't need to provide them with an explicit starting value.
-- They assume the first (or last) element of the list to be the
--  starting value and then start the fold with the element next to it.

-- head is better implemented by pattern matching, but this just goes to show,
-- you can still achieve it by using folds
head' :: [a] -> a
head' = foldr1 (\x _ -> x)

last' :: [a] -> a
last' = foldl1 (\_ x -> x)


-- Scans are used to monitor the progression of a function that can be
-- implemented as a fold.
--
-- scanl and scanr are like foldl and foldr, only they report all the
-- intermediate accumulator states in the form of a list.
-- There are also scanl1 and scanr1, which are analogous to foldl1 and foldr1
-- scanl (+) 0 [3,5,2,1]    - results in [0,3,8,10,11]
-- scanr (+) 0 [3,5,2,1]    - results in [11,8,3,1,0]

-- Wacky boilerplate to make all tests run.
return []
-- run this to test all quickcheck properties
runTests :: IO Bool
runTests = $quickCheckAll
