{-
Exercise 3 Matrix arithmetic forms a ring. Write a datatype Mat2x2
(you choose the representation) and Ring and Parsable instances.

Your parser must be able to read something like [[1,2][3,4]] as a
2 × 2 matrix. It does not need to allow for the posssibility of spaces.
Writing this idiomatically in Haskell is hard, so we will be more
forgiving about style in the matrix parser.

Test your instances!
-}

module HW05_Ex3 where

import HW05_Ring
import HW05_Parser
import Test.HUnit
import Data.Maybe    ( listToMaybe )

data Mat2x2 a = Mat2x2 a a a a
                    deriving (Eq, Show, Read)


mkMat2x2 :: Num a => a -> a -> a -> a -> Mat2x2 a
mkMat2x2 p q r s = Mat2x2 p q r s

instance Num a => Ring (Mat2x2 a) where
  addId                     = mkMat2x2 0 0 0 0
  addInv (Mat2x2 p q r s)   = mkMat2x2 (negate p) (negate q) (negate r) (negate s)
  mulId                     = mkMat2x2 1 0 0 1

  add (Mat2x2 p q r s) (Mat2x2 u v w x) = mkMat2x2 (p+u) (q+v) (r+w) (s+x)
  mul (Mat2x2 p q r s) (Mat2x2 u v w x) = mkMat2x2 (p*u + q*w) (p*v + q*x) (r*u + s*w) (p*v + q*x)

mkMat2x2Works :: Bool
mkMat2x2Works = addId == mkMat2x2 0 0 0 0 &&
            mulId == mkMat2x2 1 0 0 1 &&
            add (addInv (mkMat2x2 2 3 4 5)) (mkMat2x2 2 3 4 5) == addId &&
            True

instance (Read a) => Parsable (Mat2x2 a) where
    parse = listToMaybe . reads
{-
-- Parse 4 numbers into a Mat2x2
instance (Parsable a, Num a) => Parsable (Mat2x2 a) where
  parse ss = case (parse ss) of
                Just(p,rs) ->
                    case (parse rs) of
                        Just(q,rss) ->
                            case (parse rss) of
                                Just(r,rsss) ->
                                    case (parse rsss) of
                                        Just(s,rssss) -> Just(mkMat2x2 p q r s, rssss)

mkMat2x2ParsingWorks :: Bool
mkMat2x2ParsingWorks = lhs == Just(mkMat2x2 3 4 5 600, "x")
    where
        lhs = parse "3 4 5 600x" :: Maybe(Mat2x2 Integer,String)
-}
mkMat2x2ParsingWorks :: Bool
mkMat2x2ParsingWorks = (parse "Mat2x2 3 1 2 3" :: Maybe(Mat2x2 Integer,String)) == Just (Mat2x2 3 1 2 3,"")

testMat2x2 :: Test
testMat2x2 = TestList [
    "test with succeeding chars" ~: (Just (Mat2x2 3 1 2 3, " kids alone") :: Maybe(Mat2x2 Integer, String))  ~=? (parse "Mat2x2 3 1 2 3 kids alone"),
    "test with preceeding chars" ~: (Nothing :: Maybe(Mat2x2 Integer, String))  ~=? (parse " kids were Mat2x2 3 1 2 3"),
    "test with empty string"     ~: (Nothing :: Maybe(Mat2x2 Integer, String))  ~=? (parse ""),
    "test it as a stringxs alone"  ~: (Just (Mat2x2 3 1 2 3,""):: Maybe(Mat2x2 Integer, String)) ~=? (parse "Mat2x2 3 1 2 3"),
    "test addId" ~: (Mat2x2 0 0 0 0) ~=? (addId),
    "test mulId" ~: (Mat2x2 1 0 0 1 :: Mat2x2 Integer) ~=? (mulId),
    "test add"   ~: (Mat2x2 4 6 8 10) ~=? (add (Mat2x2 2 3 4 5) (Mat2x2 2 3 4 5)),
    "test mul"   ~: (Mat2x2 2 2 2 2)  ~=? (mul (Mat2x2 1 1 1 1) (Mat2x2 1 1 1 1))
    ]
