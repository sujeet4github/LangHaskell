module HW09_TestingBST where

-- http://www.seas.upenn.edu/~cis194/fall14/hw/09-testing.pdf

{-
Generating Binary Search Trees (Uses BST.hs from HW04)
-}

import System.Random
import Test.QuickCheck
import BST

-- Ex 7
-- already present in BST.hs
{-
instance Arbitrary a => Arbitrary (BST a) where
  arbitrary = sized genBST
-}

{-
The two parameters are the lower and upper bounds of the tree.
The function does the following:
1. First, it decides whether it will make a leaf or an interior node.
  This can be done, say, by generating an arbitrary Bool, though
  there are other Gen combinators that make this more elegant.
2. If you wish to make a leaf, do so.
3. Otherwise, generate a value of type a in the range given by the
  parameters. Call this value x.
4. Generate a tree bounded above by x via a recursive call; this
  will be your left sub-tree.
5. Generate a tree bounded below by x via a recrusive call; this
  will be your right sub-tree.
6. Put x together with the left and right sub-trees in a Node, and
  you’re done.
-}
genBST :: (Eq a, Arbitrary a, Random a) => a -> a -> Gen (BST a)
genBST lowerBound upperBound = oneof [genLeaf, genTree]
    where
        genLeaf = return Leaf
        genTree = do
                x <- choose (lowerBound, upperBound)
                leftTree <- genBST lowerBound x
                rightTree <- genBST x upperBound
                return $ Node leftTree x rightTree
