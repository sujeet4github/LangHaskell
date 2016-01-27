{-
Exercise 2 Modular arithmetic forms a ring.

We will be thinking of the integers modulo 5. This ring has 5 elements: R = {0, 1, 2, 3, 4}.

Addition is like normal integer addition, but it wraps around. So,
3 + 4 = 2 and 1 + 4 = 0. Multiplication is like normal integer
multiplication, but it, too, wraps around. Note that Haskell’s mod
function is very handy here!

Define a datatype
data Mod5 = MkMod Integer
deriving (Show, Eq)
with Ring and Parsable instances. (Your Parsable instance should
parse just like Integer’s.)
Test your instances!

-}

{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module HW05_Ex2 where

import HW05_Ring
import HW05_Parser
import Test.HUnit
import Data.Maybe    ( listToMaybe )

data Mod5 = MkMod Integer deriving (Show, Eq, Read)

instance Ring Mod5 where
  addId             = (MkMod 0)
  addInv (MkMod x)  = MkMod $ negate x
  mulId             = (MkMod 1)

  add (MkMod l) (MkMod r) = mkMod5 (l + r)
  mul (MkMod l) (MkMod r) = mkMod5 (l * r)

mkMod5 :: Integer -> Mod5
mkMod5 n = MkMod $ n `mod` 5

mod5Works :: Bool
mod5Works = addId == MkMod (0 :: Integer) &&
            mulId == MkMod (1 :: Integer) &&
            addInv (MkMod 3) == (MkMod (-3)) &&
            add (mkMod5 1) (mkMod5 0) == (MkMod 1) &&
            add (mkMod5 1) (mkMod5 1) == (MkMod 2) &&
            add (mkMod5 1) (mkMod5 4) == (MkMod 0) &&
            mul (mkMod5 1) (mkMod5 0) == (MkMod 0) &&
            mul (mkMod5 1) (mkMod5 1) == (MkMod 1) &&
            mul (mkMod5 1) (mkMod5 4) == (MkMod 4) &&
            mul (mkMod5 2) (mkMod5 3) == (MkMod 1) &&
            True

instance Parsable Mod5 where
  parse = listToMaybe . reads
-- The following was converting ints into MkMod's
--  parse s = case parse s of Just(n,rs) -> Just(mkMod5 n, rs)

mod5ParsingWorks :: Bool
mod5ParsingWorks = ((parse "MkMod 3" :: Maybe(Mod5,String)) == Just (MkMod 3, "")) &&
                (parseRing "MkMod 1 + MkMod 5" ::(Maybe Mod5)) == Just (MkMod 1) &&
                (parseRing "MkMod 1 + MkMod 2 * MkMod 5" ::(Maybe Mod5)) == Just (MkMod 1)

testMod5 :: Test
testMod5 = TestList [
  "test with succeeding chars" ~: (Just (MkMod 61, " kids alone") :: Maybe (Mod5, String))  ~=? (parse "MkMod 61 kids alone"),
  "test with preceeding chars" ~: (Nothing :: Maybe (Mod5, String))  ~=? (parse " kids were MkMod 100"),
  "test with empty string"     ~: (Nothing :: Maybe (Mod5, String))  ~=? (parse ""),
  "test it as a stringxs alone"  ~: (Just (MkMod 5, "") :: Maybe (Mod5, String))  ~=? (parse "MkMod 5"),
  "test addId" ~: (MkMod 0 :: Mod5) ~=? (addId),
  "test mulId" ~: (MkMod 1 :: Mod5) ~=? (mulId),
  "test add"   ~: (MkMod 2 :: Mod5) ~=? (add (MkMod 3) (MkMod 4)),
  "test mul"   ~: (MkMod 0 :: Mod5) ~=? (mul (MkMod 5) (MkMod 7))
  ]
