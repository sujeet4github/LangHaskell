{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

import Test.QuickCheck
import Test.QuickCheck.All

-------------------------------------------------------------------------------
-- Exercise 1: Wholemeal programming
-- reimplement in more idiomatic haskell
fun1 :: [Integer] -> Integer
fun1 [] = 1
fun1 (x:xs)
  | even x = (x - 2) * fun1 xs
  | otherwise = fun1 xs

fun2 :: Integer -> Integer
fun2 1 = 0
fun2 n  | n <= 0 = 0
        | even n = n + fun2 (n `div` 2)
        | otherwise = fun2 (3 * n + 1)

idiomatic_fun1 :: [Integer] -> Integer
idiomatic_fun1 = product . map (subtract 2) . filter even
-- idiomatic_fun1 = foldr (\n a -> a * (if (even n) then (n-2) else 1)) 1
-- the else part above was not needed:
--  so equal to filter even
--    ok to throw away the odd numbers, since they are to be treated as 1 which
--          is identity for *
--    and then apply (subtract 2) cant use (- 2) to every element
--    and then take product of all the remaining

-- similar to above
-- 0 is identity for +, so can ignore the f(1) which have result in 0
--
idiomatic_fun2 :: Integer -> Integer
idiomatic_fun2 = sum
                  -- only even numbers add up! (see defn of fun2)
                  . filter even
                  -- ok to skip 1's as fun2(1) = 0
                  . (takeWhile (/= 1))
                  -- generate list of values starting from the parameter passed in
                  . (iterate (\n -> if (even n) then (n `div` 2) else 3*n+1))
                  . (max 1)

prop_fun1 :: [Integer] -> Bool
prop_fun1 ns = (fun1 ns) == (idiomatic_fun1 ns)

prop_fun2 :: Integer -> Bool
prop_fun2 n = fun2 n == idiomatic_fun2 n
prop_fun2_with_conditionalProperty :: Integer -> Property
prop_fun2_with_conditionalProperty n = (n > 0) ==> fun2 n == idiomatic_fun2 n

-------------------------------------------------------------------------------

-- Exercise 2: Folding with trees
--
-- The height of a binary tree is the length of a path from the root to the
-- deepest node.
-- For example, the height of a tree with a single node is 0; the
-- height of a tree with three nodes, whose root has two children, is 1;
-- and so on. A binary tree is balanced if the height of its left and right
-- subtrees differ by no more than 1, and its left and right subtrees are
-- also balanced.

data Tree a = Leaf
              | Node Integer (Tree a) a (Tree a)
            deriving (Show, Eq)

-- generate a balance binary tree from a list of values using foldr
foldTree :: [a] -> Tree a
foldTree = foldr balancedInsert Leaf

getHeight :: Tree a -> Integer
getHeight (Node height _ _ _)  = height
getHeight _                    = -1

balancedInsert :: a -> Tree a -> Tree a
balancedInsert a Leaf = Node 0 Leaf a Leaf
balancedInsert a (Node h left val right) = if (lh <= rh)
                                        then Node (h+1) (balancedInsert a left) val right
                                        else Node (h+1) left val (balancedInsert a right)
                                          where lh = getHeight left
                                                rh = getHeight right

foldTree' :: [a] -> Tree a
foldTree' [] = Leaf
foldTree' xs = Node
                  calculatedHeight
                  (foldTree' $ take half xs)
                  (xs !! half)
                  (foldTree' $ drop (1+half) xs)
                where
                  len = length xs
                  half = len `div` 2
                  calculatedHeight = floor (logBase 2 (fromIntegral len)::Double)

-------------------------------------------------------------------------------

-- Exercise 3: More folds!
--
-- returns True if and only if there are an odd number of True
--  It does not matter how many False values the input list contains
xor' :: [Bool] -> Bool
xor' = odd . length . filter (== True)
prop_xor'_ex1 :: Bool
prop_xor'_ex1 = xor' [False, True, False] == True
prop_xor'_ex2 :: Bool
prop_xor'_ex2 = xor' [False, True, False, False, True] == False

-- using a fold
xor :: [Bool] -> Bool
xor = foldr (\x a -> (not x && a) || (x && not a)) False

prop_xor :: [Bool] -> Bool
prop_xor bs = xor bs == xor' bs
-------------------------------------------------------------------------------

-- Implement foldl using foldr. That is, complete the definition
myFoldl :: (a -> b -> a) -> a -> [b] -> a
myFoldl _ base [] = base
myFoldl f base (x:xs) = (myFoldl f (f base x) xs)

-- myFoldl f base xs = foldr ...

-- Implement map as a fold.
map' :: (a -> b) -> [a] -> [b]
map' f = foldr (\x xs -> f(x):xs) []

prop_map_check xs = map (* 3) xs == map' (* 3) xs



-------------------------------------------------------------------------------
-- Exercise 4: Finding primes
-- http://en.wikipedia.org/wiki/Sieve_of_Sundaram
--
--  Given an integer n, your function should
-- generate all the odd prime numbers up to 2n + 2.
--
sieveSundaram :: Integer -> [Integer]
sieveSundaram n = toremove
                    where
                      primes1 = [1..n]
                      ijs = [(i,j) | j <- [1..n], i <- [1..j]]
                      toremove = map (\(i,j) -> (i+j+(2*i*j))) ijs
                      primes2 = filter (\x -> elem x toremove) primes1

-- using a list comprehension
-- produces all possible pairs given two lists
cartProd :: [a] -> [b] -> [(a, b)]
cartProd xs ys = [(x,y) | x <- xs, y <- ys]

-- prop_cart_prod_ex1 = cartProd [1,2] ['a','b'] == [(1,'a'),(1,'b'),(2,'a'),(2,'b')]

-------------------------------------------------------------------------------
-- Wacky boilerplate to make all tests run.
return []
-- run this to test all quickcheck properties
runTests :: IO Bool
runTests = $quickCheckAll
