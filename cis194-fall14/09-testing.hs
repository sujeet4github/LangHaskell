{-
http://www.seas.upenn.edu/~cis194/fall14/lectures/09-testing.html

Suggested reading:

RWH Chapter 11: Testing and Quality Assurance, but note that the QuickCheck library’s
interface has evolved a bit since this was written
[HUnit 1.0 User’s Guide](http://www.haskell.org/haskellwiki/HUnit_1.0_User’s_Guide)

-}

{-# LANGUAGE RankNTypes #-}
import Test.QuickCheck
import Test.HUnit
import Data.List
import Control.Monad

-- sample 2 test code
-- | Assuming the input lists are sorted, combine the lists into a
-- sorted output.
merge1 :: Ord a => [a] -> [a] -> [a]
merge1 (x:xs) (y:ys)
  | x < y     = x : merge1 xs ys
  | otherwise = y : merge1 xs ys
merge1 _      _      = []

-- HUnit gives us the ability to run a function against a certain input and to observe
-- whether or not it produces the desired output.
-- to run this:
-- *Main> runTestTT test1_merge1
--
-- Using HUnit
test1_merge1 :: Test
test1_merge1 = "alternating numbers: [1,3,5] [2,4,6]" ~:
               merge1 [1,3,5] [2,4,6] ~?= [1,2,3,4,5,6]

{-
checking types of methods used above with :t
(~?=) :: (Show a, Eq a) => a -> a -> Test
(~:) :: Testable t => String -> t -> Test
runTestTT :: Test -> IO Counts

The (~?=) operator builds a Test by comparing the equality of its two operands,
which must be Showable
By convention, HUnit testing operators put a ? toward the side where the failure
might occur – that’s how HUnit knew which value was expected and which was the
actual result. HUnit also provides (~=?) which swaps the arguments

The (~:) operator serves only to label Tests with a string describing the test.

Also, note that (~?=) has a higher precedence than (~:), meaning that we don’t
need parentheses. (You can query the precedence of an operator in GHCi.
If I say :info ~:, I get infixr 0 ~:, which means that (~:) associates to the right
with the lowest possible precedence.

runTestTT is the function that runs tests and produces output. It also returns a
Counts object, which is printed in the last line of the GHCi transcript above.
It contains a summary of the testing that can then be operated on programmatically,
if runTestTT is called from a larger program’s main action.

HUnit actually exports the details of its Test type:

data Test
  = TestCase Assertion
  | TestList [Test]
  | TestLabel String Test
You can build up tests using these constructors.
-}
test2_merge1 :: Test
test2_merge1 = TestList [ "one element: [1] []" ~:
                          merge1 [1] [] ~?= [1]
                        , test1_merge1 ]


-- generalizing tests: so it can run on any merge
type MergeFun = forall a. Ord a => [a] -> [a] -> [a]
test2 :: MergeFun -> Test
test2 merge = TestList [ "one element: [1] []" ~:
                         merge [1] [] ~?= [1]
                       , "alternating numbers: [1,3,5] [2,4,6]" ~:
                         merge [1,3,5] [2,4,6] ~?= [1,2,3,4,5,6]
                       ]

merge2 :: MergeFun
merge2 all_xs@(x:xs) all_ys@(y:ys)
  | x < y     = x : merge2 xs all_ys
  | otherwise = y : merge2 all_xs ys
merge2 _             _             = []

merge3 :: MergeFun
merge3 all_xs@(x:xs) all_ys@(y:ys)
  | x < y     = x : merge3 all_ys xs
  | otherwise = y : merge3 all_xs ys
merge3 xs            []            = xs
merge3 _             _             = []


{-
QuickCheck is for property based testing.
Writing test cases is boring. And, it’s easy to miss out on unexpected behavior.
Much better (and, more along the lines of wholemeal programming) is to define
properties we wish our function to have. Then, we can get the computer to generate
the test cases for us.
-}
-- property being checked:
-- sum of the lengths of the input lists should be same as length of output list
prop_numElements_merge3 :: [Integer] -> [Integer] -> Bool
prop_numElements_merge3 xs ys
  = length xs + length ys == length (merge3 xs ys)
-- run using
-- *Main> quickCheck prop_numElements_merge3

prop_numElements :: MergeFun -> [Integer] -> [Integer] -> Bool
prop_numElements merge xs ys
  = length xs + length ys == length (merge xs ys)

-- And, we take another stab at our function:
merge4 :: MergeFun
merge4 all_xs@(x:xs) all_ys@(y:ys)
  | x < y     = x : merge4 xs all_ys
  | otherwise = y : merge4 all_xs ys
merge4 xs            ys            = xs ++ ys

{-
Looking at the types:
quickCheck :: Test.QuickCheck.Testable prop => prop -> IO ()

we can quickCheck any thing that is Testable,
    ** booleans are testable
    instance [safe] Test.QuickCheck.Testable Bool
    instance [safe] Test.QuickCheck.Testable Property
    ** says, a function is testable, if
    **  1. argument is Arbitrary and Show -able
    **  2. result is Testable
    instance [safe] (Arbitrary a, Show a,
                     Test.QuickCheck.Testable prop) =>
                Test.QuickCheck.Testable (a -> prop)


Generating Arbitrary Data
=========================

When you want to use QuickCheck over your own datatypes, it is necessary to
write an Arbitrary instance for them.

-}
data MyList a = Nil | Cons a (MyList a)

instance Show a => Show (MyList a) where
  show = show . toList

toList :: MyList a -> [a]
toList Nil           = []
toList (a `Cons` as) = a : toList as

fromList :: [a] -> MyList a
fromList []     = Nil
fromList (a:as) = a `Cons` fromList as

instance Arbitrary a => Arbitrary (MyList a) where
  arbitrary = genMyList

-- to test this generator:
-- repl> sample (genMyList :: Gen (MyList Integer))
genMyList :: Arbitrary a => Gen (MyList a)
genMyList = do
    make_nil <- arbitrary
    if make_nil
        then return Nil
        else do
            x <- arbitrary
            xs <- genMyList
            return (x `Cons` xs)

