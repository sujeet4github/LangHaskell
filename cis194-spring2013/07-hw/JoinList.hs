{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module JoinList where

import Data.Monoid
import Sized
import Scrabble
import Buffer
import Editor
import Test.QuickCheck

-- The m parameter will be used to track monoidal annotations to the
-- structure. The idea is that the annotation at the root of a JoinList
-- will always be equal to the combination of all the annotations on
-- the Single nodes (according to whatever notion of “combining” is
-- defined for the monoid in question). Empty nodes do not explicitly
-- store an annotation, but we consider them to have an annotation of
-- mempty (that is, the identity element for the given monoid).
data JoinList m a = Empty
      | Single m a
      | Append m (JoinList m a) (JoinList m a)
      deriving (Eq, Show)

------------------------------------------------------------------
-- Exercise 1
------------------------------------------------------------------
-- gets the annotation at the root of a JoinList
tag :: Monoid m => JoinList m a -> m
tag Empty       = mempty
tag (Single m _)  = m
tag (Append m _ _) = m

-- yields a new JoinList whose monoidal annotation is derived
-- from those of the two arguments.
(+++) :: Monoid m => JoinList m a -> JoinList m a -> JoinList m a
left +++ right = Append (tag left <> tag right) left right

------------------------------------------------------------------
-- Exercise 2
------------------------------------------------------------------
-- The first annotation to try out is one for fast indexing
-- into a JoinList.
-- The idea is to cache the size (number of data elements)
-- of each subtree. This can then be used at each step to
-- determine if the desired index is in the left or the right
--  branch.
-- Use the Sized type class to write the following functions
-- 1. finds the JoinList element at the specified index.
indexJ :: (Sized b, Monoid b) =>
            Int -> JoinList b a -> Maybe a
indexJ _ Empty      = Nothing
indexJ i (Single _ a) = if i == 0 then Just a else Nothing
indexJ i (Append j l r)
          | i < leftSize    = indexJ i l
          | otherwise       = indexJ (i - leftSize) r
          where
            leftSize = getSize . size . tag $ l

indexJv2 :: (Sized b, Monoid b) =>
            Int -> JoinList b a -> Maybe a
indexJv2 _ Empty     = Nothing
indexJv2 index (Single _ a) = if index == 0 then Just a else Nothing
indexJv2 index (Append m left right) = case compare index sizeLeft of
                  LT -> indexJv2 index left
                  GT -> indexJv2 (index - sizeLeft) right
                  EQ -> indexJv2 (index - sizeLeft) right
                  where
                    sizeLeft = getSize . size . tag $ left

-- tests
prop_indexJ_testList :: Bool
prop_indexJ_testList = (indexJ 1 testList) == (jlToList testList !!? 1)
prop_indexJ :: Int -> TestJLT1 -> Bool
prop_indexJ n xs = (indexJv2 n xs) == (jlToList xs !!? n)
                    && (indexJ n xs) == (jlToList xs !!? n)

--prop_indexJ_2 :: Int -> [[Char]] -> Bool
--prop_indexJ_2 n xs = jl

-- 2. drops the first n elements from a JoinList
-- This is analogous to the standard drop function on lists.
-- Formally,
--  dropJ should behave in such a way that
--  jlToList (dropJ n jl) == drop n (jlToList jl)
dropJ :: (Sized b, Monoid b) =>
            Int -> JoinList b a -> JoinList b a
dropJ i x | i <= 0 = x
dropJ i x@(Append j l r)
        | i >= centerSize = Empty
        | i >= leftSize   = dropJ (i - leftSize) r
        | i < leftSize    = (dropJ i l) +++ r
        where
          centerSize = getSize . size $ j
          leftSize = getSize . size . tag $ l
          rightSize = getSize . size . tag $ r

dropJv2 :: (Sized b, Monoid b) =>
            Int -> JoinList b a -> JoinList b a
dropJv2 _ Empty     = Empty
dropJv2 i x | i <= 0 = x
dropJv2 _ (Single _ _) = Empty
dropJv2 index (Append m l r) = case compare index centerSize of
              GT -> Empty
              EQ -> Empty
              LT -> case compare index leftSize of
                GT -> dropJv2 (index - leftSize) r
                EQ -> r
                LT -> (dropJv2 index l) +++ r
              where
                centerSize = getSize . size $ m
                leftSize = getSize . size . tag $ l

-- tests
prop_test_dropJ_testList_n :: Int -> Property
prop_test_dropJ_testList_n n = jlToList (dropJ n testList) == drop n (jlToList testList)
                              ==> jlToList (dropJv2 n testList) == drop n (jlToList testList)
prop_test_dropJ_withJL :: TestJLT1 -> Int -> Property
prop_test_dropJ_withJL jl n = jlToList (dropJv2 n jl) == drop n (jlToList jl)
                              ==> jlToList (dropJv2 n jl) == drop n (jlToList jl)

-- 3. returns the first n elements of a JoinList
takeJ :: (Sized b, Monoid b) =>
          Int -> JoinList b a -> JoinList b a
takeJ i x | i <= 0 = Empty
takeJ i x | i >= centerSize = x
              where centerSize = getSize . size . tag $ x
takeJ i x@(Append j l r)
          | i > leftSize = l +++ (takeJ (i-leftSize) r)
          | otherwise = takeJ i l
          where
              leftSize = getSize . size . tag $ l
              rightSize = getSize . size . tag $ r

-- tests
prop_test_takeJ_testList_n :: Int -> Bool
prop_test_takeJ_testList_n n = jlToList (takeJ n testList) == take n (jlToList testList)
prop_test_takeJ_withJL :: TestJLT1 -> Int -> Bool
prop_test_takeJ_withJL jl n = jlToList (takeJ n jl) == take n (jlToList jl)

------------------------------------------------------------------
-- Testing helpers for Exercise 2
------------------------------------------------------------------
--  converting join-lists into lists
--
jlToList :: JoinList m a -> [a]
jlToList Empty = []
jlToList (Single _ a) = [a]
jlToList (Append _ l r) = jlToList l ++ jlToList r
-- safe list indexing
--
(!!?) :: [a] -> Int -> Maybe a
[] !!? _ = Nothing
_ !!? i | i < 0 = Nothing
(x:xs) !!? 0 = Just x
(x:xs) !!? i = xs !!? (i-1)
-- simple test list
--
testList :: JoinList Size String
testList = (Append (Size 4)
                (Append (Size 2)
                    (Single (Size 1) "trick joke")
                    (Single (Size 1) "happy dude")
                )
                (Append (Size 2)
                    (Single (Size 1) "smile corn")
                    (Single (Size 1) "drown duck")
                )
           )

-- quickcheck generator
--
type TestJLT1 = JoinList Size String
sizedArbTestJLT1 :: Int -> Gen TestJLT1
sizedArbTestJLT1 n
                  | n <= 0  =   do
                                  s <- arbitrary
                                  return $ Single (Size 1) s
                  | otherwise = do
                                  s <- arbitrary
                                  subListCount <- choose(0, n-1)
                                  subList <- sizedArbTestJLT1 subListCount
                                  return $ (Single (Size 1) s) +++ subList
--
instance Arbitrary TestJLT1 where
  arbitrary = sized sizedArbTestJLT1
--
-- tests should be with this signature
-- prop_appendPath_does_something :: TestJLT1 -> Property
-- prop_appendPath_does_something t = undefined -- stub
-- We can sample the test data that's generated like so:
-- sample (sizedArbTestJLT1 2)
--
-- quickcheck generator <END>
--

------------------------------------------------------------------
-- Exercise 3
------------------------------------------------------------------
-- the second annotation you decide to implement is one
-- to cache the ScrabbleTM score for every line in a buffer.
-- Create a Scrabble module that defines a Score type, a
-- Monoid instance for Score, and the following functions:

scoreLine :: String -> JoinList Score String
scoreLine s = Single (scoreString s) s

------------------------------------------------------------------
-- Exercise 4
------------------------------------------------------------------
-- Finally, combine these two kinds of annotations. A pair
-- of monoids is itself a monoid.
{-
instance (Monoid a, Monoid b) => Monoid (a,b) where
  mempty = (mempty, mempty)
  mappend (a1,b1) (a2,b2) = (mappend a1 a2, mappend b1 b2)
defined in Data.Monoid
-}
-- JoinList (Score, Size) String, allows us to track both size and score

convLine :: String -> JoinList (Score, Size) String
convLine s = Single (scoreString s, 1) s

convLines :: [String] -> JoinList (Score, Size) String
convLines [] = Single (Score 0,Size 1) ""
convLines xs = foldr1 (+++) convLineMappedList
              where convLineMappedList = map convLine xs

convMultiLineString :: String -> JoinList (Score, Size) String
convMultiLineString = convLines . lines

instance Buffer (JoinList (Score, Size) String) where
  toString      =  show
  fromString    = convMultiLineString
  line n b      = indexJ n b
  replaceLine n l b = takeJ (n-1) b +++ (convLine l) +++ (dropJ n b)
  numLines      = getSize . size . tag
  value         = getScore . fst . tag

prop_toStringWithConvLines :: [String] -> Bool
prop_toStringWithConvLines xs = lhs == rhs
                where
                  lhs = foldr (++) "" xs
                  rhs = toString . convLines $ xs

prop_fromString :: String -> Bool
prop_fromString s = s == toString (fromString s :: JoinList (Score, Size) String)

testListAsBuffer ::  JoinList (Score, Size) String
testListAsBuffer = convLines . jlToList $ testList
prop_line_testListAsBuffer :: Bool
prop_line_testListAsBuffer = (line 1 testListAsBuffer) == Just "happy dude"
-- prop_line :: Int -> [[Char]] -> Property
-- prop_line n xs = (n < 0 || n > sizeJL) ==> lineAt == Nothing
--                   where
--                     buffer =
--                     sizeJL = getSize . size . tag $ buffer
--                     lineAt = (line n jl) :: Maybe String


main = runEditor editor $ convLines
           [ "This buffer is for notes you don't want to save, and for"
           , "evaluation of steam valve coefficients."
           , "To load a different file, type the character L followed"
           , "by the name of the file."
           ]
