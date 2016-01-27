{-
Exercise 6 Write squashMulId that detects whenever you are multiplying
(on either side) by the multiplicative identity, and remove the
multiplication. To get this working over parsed expressions is a little
tricky, because the parser does not produce MulId. For example, in a
RingExpr Integer, the multiplicative identity would look like Lit 0.
Bonus brownie points1
if you avoid using eval.
1 These are not real points, but it would make us happy!

Test your function!
-}

module HW05_Ex6 where

import HW05_Ring
import HW05_Parser



squashMulId :: RingExpr a -> RingExpr a
squashMulId (Lit a)         = Lit a
squashMulId AddId           = AddId
squashMulId (AddInv x)      = AddInv (squashMulId x)
squashMulId MulId           = MulId
squashMulId (Add x y)       = (Add (squashMulId x) (squashMulId y))
squashMulId (Mul MulId x)   = (squashMulId x)
squashMulId (Mul x MulId)   = (squashMulId x)
squashMulId (Mul x y)       = (Mul (squashMulId x) (squashMulId y))
