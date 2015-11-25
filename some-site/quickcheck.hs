
-- Consider a very simple property from number theory:
-- “For each integer n, if n is even, then n+1 is odd.”
--
import Test.QuickCheck

-- Link1: http://matt.might.net/articles/quick-quickcheck/
-- Link2: http://www.stackbuilders.com/news/a-quickcheck-tutorial-generators
-----------------------------------------------------------------------------
-- http://matt.might.net/articles/quick-quickcheck/
-- TODO: futher advanced examples on testing/generating red-black trees
-- Monadic testing using QuickCheck
-- https://www.fpcomplete.com/user/christianpbrink/quickcheck-and-webdriver

-- quickCheck(\n -> even(n) ==> odd(n+1))
prop_1 :: Integer -> Property
prop_1 n = even(n) ==> odd(n+1)

-- Collatz conjecture
-- f(n)=n/2 if n is even
-- f(n)=3n+1 if n is odd
f :: Integer -> Integer
f n | even n = n `div` 2
    | otherwise = 3*n + 1
-- The Collatz conjecture states that repeating this process eventually reaches 1.
collatzMet :: Integer -> Bool
collatzMet 1 = True
collatzMet x = collatzMet . f $ x
prop_collatzMet_1 :: Integer -> Property
prop_collatzMet_1 n = n > 0 ==> collatzMet n

-- Instead of allowing QuickCheck to generate test cases that are never used,
-- we can instead write a generator, positives, that only yields
-- random positive values:
positives :: Gen Integer
-- Gen is a monadic kind for describing generators of random values.
{-
arbitrary is a member of the Arbitrary typeclass, and it yields a generator
for the parameter:
class Arbitrary a where
  arbitrary :: Gen a
QuickCheck instantiates Arbitrary on many of the basic types like Integer.
-}
positives = do
              aNumber <- arbitrary
              if aNumber == 0
                then return 1
                else return . abs $ aNumber

prop_collatzMet_2 :: Property
-- The forAll quantifier will draw test cases only from a specified generator:
prop_collatzMet_2 = forAll positives collatzMet

-----------------------------------------------------------------------------
-- The default generator for values of any type is arbitrary, which is a method
-- of QuickCheck's Arbitrary type class:
--
-- Let's run arbitrary to generate values of some basic types that have an
-- instance of Arbitrary:
gen_i = generate arbitrary :: IO Int
gen_tup = generate arbitrary :: IO (Char, Bool)
gen_maybe_list = generate arbitrary :: IO [Maybe Bool]
gen_either = generate arbitrary :: IO (Either Int Double)

-- Additionally, QuickCheck provides several combinators that we can use to
-- generate random values and define our own instances of Arbitrary
-- combinator ex #1 - choose
dice :: Gen Int
dice =
  choose (1, 6)
sample_rolls_of_dice = sample dice
random_roll = generate dice
-- combinator ex #2 - sized
-- Let's use (rose) trees as an example of how to do this:
-- A rose tree is just a node and a list of trees.
data RTree a
  = N a [RTree a]
  deriving (Show)
-- example
aTree :: RTree Int
aTree =
  N 5 [N 12 [N (-16) []],N 10 [],N 16 [N 12 []]]
-- Given such a tree, we can ask for things such as
-- the number of nodes / number of edges
-- height of the tree
nodes :: RTree a -> Int
nodes rt = 1 + nodes_children
            where
              nodes_on_children = map nodes (children rt)
              nodes_children = foldr (\x z -> z+x) 0 nodes_on_children
height :: RTree a -> Int
height rt = 1 + height_children
            where
              height_children = foldr max 0 (map height (children rt))
edges :: RTree a -> Int
edges (N _ st) = length st + edges_children
            where
              edges_on_children = map edges st
              edges_children = sum edges_on_children
children :: RTree a -> [RTree a]
children (N _ []) = []
children (N _ xs) = xs

-- Given definitions for nodes and edges, we can test that they satisfy
-- the theorem that every tree has one more node than it has edges:
prop_OneMoreNodeThanEdges :: RTree Int -> Bool
prop_OneMoreNodeThanEdges tree =
  nodes tree == edges tree + 1
-- But Tree a is not an instance of Arbitrary yet, so QuickCheck doesn't know
-- how to generate values to check the property.
-- We could simply use the arbitrary generator for lists:
{-
instance Arbitrary a => Arbitrary (RTree a) where
  arbitrary = do
    t <- arbitrary
    ts <- arbitrary
    return (N t ts)
-}
-- But we wouldn't be able to guarantee that such a generator would ever stop.
-- Thus, we need to use the sized combinator:
instance Arbitrary a => Arbitrary (RTree a) where
  arbitrary = sized arbitrarySizedRTree

-- Given a size parameter m,
--    we generate a value of type a,
--       choose a number n to be the number of trees in the list,
--       and then generate n trees using the vectorOf combinator.
-- We use the div function to make sure that generation stops at some point.
arbitrarySizedRTree :: Arbitrary a => Int -> Gen (RTree a)
arbitrarySizedRTree m = do
  t <- arbitrary
  n <- choose (0, m `div` 2)
  ts <- vectorOf n (arbitrarySizedRTree (m `div` 4))
  return (N t ts)
-- To test the generator
-- generate arbitrary :: IO (Tree Int)
