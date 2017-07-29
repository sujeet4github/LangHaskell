module RunLength (decode, encode) where
import Data.Foldable     (for_)
import Data.Char(isDigit, digitToInt)

decode :: String -> String
decode encodedText = decodeCountedChars $ encodedStringToCountedChars encodedText

encode :: String -> String
encode text = countedChars2EncodedString $ encodeToCountedChars text NoChar []

-- 1st def of encodeToCountedChars
test1_1_e1 = encodeToCountedChars "" NoChar []
test2_1_e1 = encodeToCountedChars "" NoChar [Counted 24 'x', Counted 5 'y']
-- 2nd def of encodeToCountedChars
test3_2_e1 = encodeToCountedChars "" (Counted 10 'p') []
test4_2_e1 = encodeToCountedChars "" (Counted 10 'p') [Counted 24 'x', Counted 5 'y']
-- 3rd def of encodeToCountedChars
test5_3_e1 = encodeToCountedChars "a" NoChar []
-- 4th def of encodeToCountedChars
test6_4_e1 = encodeToCountedChars "a" (Counted 4 'a') [Counted 24 'x', Counted 5 'y']
-- 5th def of encodeToCountedChars
test7_5_e1 = encodeToCountedChars "a" (Counted 4 'b') [Counted 24 'x', Counted 5 'y']
-- overall test of encode
test_O1_e1 = encodeToCountedChars "  hsqq qww  " NoChar []

encodeToCountedChars :: String -> CountedChar -> [CountedChar] -> [CountedChar]
-- 1st
encodeToCountedChars [] NoChar res      = res
-- 2nd
encodeToCountedChars [] cc res          = cc : res
-- 3rd
encodeToCountedChars (x:xs) NoChar []   = encodeToCountedChars xs (Counted 1 x) []
-- 4th
encodeToCountedChars (x:xs) cc@(Counted n y) res
  | x == y        = encodeToCountedChars xs (Counted (n+1) x) res
-- 5th
  | otherwise     = encodeToCountedChars xs (Counted 1 x) res2
                      where
                        res2 = cc : res

-- Overall tests
--
test_O4 = for_ encodeTestCases $ test encode
  where
    test f kase = putStrLn(
      "encode of ["
      ++ input kase
      ++ "] is ["
      ++ (encode $ input kase)
      ++ "] expected ["
      ++ expected kase
      ++ "]"
      ++ "\ndecode of ["
      ++ expected kase
      ++ "] is ["
      ++ (decode $ expected kase)
      ++ "] expected ["
      ++ input kase
      ++ "]"
       )

------ Test Cases
data Case = Case { description :: String
                 , input       :: String
                 , expected    :: String
                 }

encodeTestCases :: [Case]
encodeTestCases =
    [ Case { description = "encode empty string"
           , input       = ""
           , expected    = ""
           }
    , Case { description = "encode single characters only"
           , input       = "XYZ"
           , expected    = "XYZ"
           }
    , Case { description = "encode simple"
           , input       = "AABBBCCCC"
           , expected    = "2A3B4C"
           }
    , Case { description = "encode with single values"
           , input       = "WWWWWWWWWWWWBWWWWWWWWWWWWBBBWWWWWWWWWWWWWWWWWWWWWWWWB"
           , expected    = "12WB12W3B24WB"
           }
    , Case { description = "encode whitespace"
           , input       = "  hsqq qww  "
           , expected    = "2 hs2q q2w2 "
           }
    , Case { description = "encode lowercase"
           , input       = "aabbbcccc"
           , expected    = "2a3b4c"
           }
     , Case { description = "mix case z's"
            , input       = "zzz ZZ  zZ"
            , expected    = "3z 2Z2 zZ"
            }
    ]

-- ---------------------- Data Structure
data CountedChar =  NoChar |
                    Counted { count :: Int,
                              chr :: Char
                            } deriving (Show, Eq)

countAChar :: Int -> Char -> CountedChar
countAChar n chr
  | n >= 1 = Counted n chr
  | otherwise = error "CountedChar's does not support this value of n"

countedChars2EncodedString :: [CountedChar] -> String
countedChars2EncodedString ccs = foldl (++) "" $ map toStrEach $ reverse ccs
  where
    toStrEach (Counted 1 c) = [c]
    toStrEach cc = (show $ count cc) ++ [chr cc]

decodeCountedChars :: [CountedChar] -> String
decodeCountedChars xs = foldr (++) "" $ map decodeOne $ reverse xs
  where
    decodeOne (Counted n c) = replicate n c

-- ----------------- Parsing - 1 into Data Structure

encodedStringToCountedChars :: String -> [CountedChar]
encodedStringToCountedChars text = parse text 0 []
  where
    parse [] 0 acc = acc
    parse [] _ acc = error "parsing error - number cannot be at end"
    parse (x:xs) n acc
      | (isDigit x) == True = parse xs (n*10 + (digitToInt x)) acc
      | otherwise           = parse xs 0 ((Counted count x):acc)
          where
            count = if (n > 0) then n else 1
