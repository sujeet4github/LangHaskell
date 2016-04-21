module HW09_TestingRingUsingProperties where

-- http://www.seas.upenn.edu/~cis194/fall14/hw/09-testing.pdf

{-
In our case, you will be testing to
make sure that Ring instances (provided, in Ring.hs, available from
the website) obey the ring properties.
-}

import Test.QuickCheck
import Ring


-- Ex 1
-- If you want to test the rings, you will need Arbitrary
-- instances for Mod5 and Mat2x2 so that QuickCheck can create
-- arbitrary values of these types
instance Arbitrary Mod5 where
    arbitrary = do
        i <- arbitrary
        return $ mkMod i
-- *> sample (arbitrary :: Gen Mod5)

tuple2Mat2x2 :: (Integer,Integer,Integer,Integer) -> Mat2x2
tuple2Mat2x2 (x1, x2, x3, x4) = MkMat x1 x2 x3 x4

tupleFromMat2x2 :: Mat2x2 -> (Integer,Integer,Integer,Integer)
tupleFromMat2x2 (MkMat x1 x2 x3 x4) = (x1, x2, x3, x4)

-- to test
-- *> sample (arbitrary :: Gen Mat2x2)
instance Arbitrary Mat2x2 where
    arbitrary = tuple2Mat2x2 <$> arbitrary
    shrink mat = map tuple2Mat2x2 $ shrink (tupleFromMat2x2 mat)

-- Ex 2
-- Implement the shrink method for Mat2x2 by reading the shrink documentation.


-- Ex 3
-- Implement the 9 properties of Rings
--  https://en.wikipedia.org/wiki/Ring_(mathematics)
--
-- 1
prop_ring_isAnAbelianGroupUnderAddition_plusIsAssociative :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_ring_isAnAbelianGroupUnderAddition_plusIsAssociative x y z =
    add (add x y) z == add x (add y z)
-- 2
prop_ring_isAnAbelianGroupUnderAddition_plusIsCommutative :: (Ring a, Eq a) => a -> a -> Bool
prop_ring_isAnAbelianGroupUnderAddition_plusIsCommutative x y =
    add x y == add y x
-- 3
prop_ring_isAnAbelianGroupUnderAddition_existsIdentityForPlus :: (Ring a, Eq a) => a -> Bool
prop_ring_isAnAbelianGroupUnderAddition_existsIdentityForPlus x = add zero x == x
    where
        zero = addId
-- 4
prop_ring_isAnAbelianGroupUnderAddition_existsInverseForPlus :: (Ring a, Eq a) => a -> Bool
prop_ring_isAnAbelianGroupUnderAddition_existsInverseForPlus x = add x' x == zero
    where
        x' = addInv x
        zero = addId
-- 5
prop_ring_isMonoidUnderMultiplication_isAssociative :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_ring_isMonoidUnderMultiplication_isAssociative x y z =
    mul (mul x y) z == mul x (mul y z)
-- 6
prop_ring_isMonoidUnderMultiplication_existsIdentityForMult :: (Ring a, Eq a) => a -> Bool
prop_ring_isMonoidUnderMultiplication_existsIdentityForMult x = mul one x == x
    where
        one = mulId
-- 7
prop_ring_MultDistributesOverAddFromLeft :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_ring_MultDistributesOverAddFromLeft x y z =
    mul x (add y z) == add (mul x y)(mul x z)

-- 8
prop_ring_MultDistributesOverAddFromRight :: (Ring a, Eq a) => a -> a -> a -> Bool
prop_ring_MultDistributesOverAddFromRight  x y z =
    mul (add x y) z == add (mul x z)(mul y z)

prop_ringIsAbelianGroupUnderAddition :: (Eq a, Ring a) => a -> a -> a -> Property
prop_ringIsAbelianGroupUnderAddition x y z =
   prop_ring_isAnAbelianGroupUnderAddition_plusIsAssociative x y z
   .&&. prop_ring_isAnAbelianGroupUnderAddition_plusIsCommutative x y
   .&&. prop_ring_isAnAbelianGroupUnderAddition_existsIdentityForPlus x
   .&&. prop_ring_isAnAbelianGroupUnderAddition_existsInverseForPlus x

prop_ringIsMonoidUnderMultiplication :: (Eq a, Ring a) => a -> a -> a -> Property
prop_ringIsMonoidUnderMultiplication x y z =
    prop_ring_isMonoidUnderMultiplication_isAssociative x y z
    .&&. prop_ring_isMonoidUnderMultiplication_existsIdentityForMult x

prop_ring_MultDistributesOnAddition :: (Eq a, Ring a) => a -> a -> a -> Property
prop_ring_MultDistributesOnAddition x y z =
    prop_ring_MultDistributesOverAddFromLeft x y z
    .&&. prop_ring_MultDistributesOverAddFromRight x y z

prop_ring :: (Eq a, Ring a) => a -> a -> a -> Property
prop_ring x y z = conjoin [
                                prop_ringIsAbelianGroupUnderAddition x y z,
                                prop_ringIsMonoidUnderMultiplication x y z,
                                prop_ring_MultDistributesOnAddition x y z
                                ]

prop_ring4Mat2x2 :: Mat2x2 -> Mat2x2 -> Mat2x2 -> Property
prop_ring4Mat2x2 = prop_ring

prop_ring4Mod5 :: Mod5 -> Mod5 -> Mod5 -> Property
prop_ring4Mod5 = prop_ring

