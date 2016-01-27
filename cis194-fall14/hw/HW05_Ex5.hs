{-
Exercise 5 Write distribute that distributes any use of multiplication
over addition. Make sure to handle both left-distribution and
right-distribution.

Test your function!

-}

module HW05_Ex5 where

import HW05_Ring
import HW05_Parser
import Test.HUnit

distribute :: RingExpr a -> RingExpr a
distribute (Lit a)         = Lit a
distribute AddId           = AddId
distribute (AddInv x)      = AddInv (distribute x)
distribute MulId           = MulId
distribute (Add x y)       = (Add (distribute x) (distribute y))
distribute (Mul x (Add y z)) = Add (Mul (distribute x) (distribute y)) (Mul (distribute x) (distribute z))
distribute (Mul (Add y z) x) = Add (Mul (distribute y) (distribute x)) (Mul (distribute z) (distribute x))
distribute (Mul x y)       = (Mul (distribute x) (distribute y))

testDistribute :: Test
testDistribute = TestList [
{-
    "distribute on Lit "    ~: Lit 10           ~=? distribute (Lit 10),
    "distribute on AddId"   ~: AddId            ~=? distribute (AddId),
    "distribute on AddInv"  ~: (AddInv 10)      ~=? distribute (AddInv 10),
    "distribute on MulId"   ~: MulId            ~=? distribute (MulId)
    -}
    ]

