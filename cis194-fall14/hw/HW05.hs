-- http://www.seas.upenn.edu/~cis194/fall14/hw/05-type-classes.pdf

module HW05 where

import HW05_Ring
import HW05_Parser
import HW05_Ex2
import HW05_Ex3
import HW05_Ex4
import Test.HUnit

{-
Rings
=====
A ring is a mathematical structure obeying certain laws. The Wikipedia
page at http://en.wikipedia.org/wiki/Ring_(mathematics) is a
great introduction. To help frame the concept, it is helpful to know
that the integers form a ring.

A ring has a carrier set R (such as the integers), an addition operation,
and a multiplication operation. In this document, we write the
addition operation with + and the multiplication operation with ×.

The operations obey the following laws:
1. + is associative. That is, (a + b) + c = a + (b + c), for all a, b, and c in R.
2. There exists a special element 0 in R such that 0 + a = a for all a in R.
    0 is the additive identity.
3. For every element a in R, there exists an element -a such that -a + a = 0.
    -a is the additive inverse of a.
4. + is commutative. That is, a + b = b + a for all a, b in R.
5. · is associative. That is, (a · b) · c = a · (b · c) for all a, b, c in R.
6. There exists a special element 1 in R such that 1 · a = a and a · 1 = a for all a in R.
    1 is the multiplicative identity.
7. · distributes over +.
    That is a · (b + c) = (a · b) + (a · c) and (b + c) · a = (b · a) + (c · a)
    for all a, b, c in  R.

Note that a ring does not require a multiplicative inverse nor does
it require · to be commutative. (A ring with a multiplicative inverse
for every non-0 element and with a commutative · is called a field, but
that’s a story for another day.)

-}

{-
Exercise 1 Homeworks are starting to get more complicated! Though
we haven’t covered any Haskell testing framework yet (HUnit is probably
the simplest), it’s time to start testing your code. For this assignment,
every exercise should be accompanied by a few definitions that
show us that your definitions work.

For example, to show that the definitions for Integer work, I could have these:

intParsingWorks :: Bool
intParsingWorks = (parse "3" == Just (3 :: Integer, "")) &&
                (parseRing "1 + 2 * 5" == Just (11 :: Integer)) &&
                (addId == (0 :: Integer))

Note that I needed to add type signatures to my numbers to let
GHC know that I wanted to talk about Integer — which has a Ring
instance — and not about other number types, like Int or Double,
which do not have Ring instances.
Now, I can just check that intParsingWorks is True in GHCi.
Make sure to include comments explaining how to use your
testing definitions!

-}
intParsingWorks :: Bool
intParsingWorks = (parse "3" == Just (3 :: Integer, "")) &&
                (parseRing "1 + 2 * 5" == Just (11 :: Integer)) &&
                (addId == (0 :: Integer))


-- To test:
-- at ghci prompt:
--  runTestTT testInteger
--

testInteger :: Test
testInteger = TestCase $ assertEqual "test addition id is 0" (0 :: Integer) addId

testIntegerII :: Test
testIntegerII = TestList [
  "test parseRing" ~: (Just 12 :: Maybe Integer)                 ~=? (parseRing "3 + 3* 3"),
  "test parse"     ~: (Just (3, "") :: Maybe (Integer, String))  ~=? (parse "3"),
  "test addId"     ~: (0 :: Integer)                             ~=? (addId),
  "test mulId"     ~: (1 :: Integer)                             ~=? (mulId),
  "test add"       ~: (4 :: Integer)                             ~=? (add 3 1),
  "test mul"       ~: (35 :: Integer)                            ~=? (mul 5 7)
  ]


