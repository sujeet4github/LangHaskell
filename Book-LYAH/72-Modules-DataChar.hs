-- http://learnyouahaskell.com/modules#data-char
--

{-
Data.Char exports a bunch of predicates over characters.
That is, functions that take a character and tell us whether some
assumption about it is true or false.

All these have type signature
    :: Char -> Bool


isControl checks whether a character is a control character.

isSpace checks whether a character is a white-space characters.
    That includes spaces, tab characters, newlines, etc.

isLower checks whether a character is lower-cased.

isUpper checks whether a character is upper-cased.

isAlpha checks whether a character is a letter.

isAlphaNum checks whether a character is a letter or a number.

isPrint checks whether a character is printable. Control characters, for instance, are not printable.

isDigit checks whether a character is a digit.

isOctDigit checks whether a character is an octal digit.

isHexDigit checks whether a character is a hex digit.

isLetter checks whether a character is a letter.

isMark checks for Unicode mark characters. Those are characters that combine with
    preceding letters to form latters with accents.

isNumber checks whether a character is numeric.

isPunctuation checks whether a character is punctuation.

isSymbol checks whether a character is a fancy mathematical or currency symbol.

isSeparator checks for Unicode spaces and separators.

isAscii checks whether a character falls into the first 128 characters of the Unicode character set.

isLatin1 checks whether a character falls into the first 256 characters of Unicode.

isAsciiUpper checks whether a character is ASCII and upper-case.

isAsciiLower checks whether a character is ASCII and lower-case.


ord and chr functions convert characters to their corresponding numbers and vice versa
    The difference between the ord values of two characters is equal to how far apart they
    are in the Unicode table.


The Data.Char also exports a datatype that's kind of like Ordering.
It's a sort of enumeration. It presents us with a few possible categories that a character can
fall into. The main function for getting the general category of a character is generalCategory.
It has a type of

generalCategory :: Char -> GeneralCategory.

There are about 31 categories.
map generalCategory " \t\nA9?|"


toUpper converts a character to upper-case. Spaces, numbers, and the like remain unchanged.

toLower converts a character to lower-case.

toTitle converts a character to title-case. For most characters, title-case is the same as upper-case.

digitToInt converts a character to an Int. To succeed, the character must be in the
    ranges '0'..'9', 'a'..'f' or 'A'..'F'.

intToDigit is the inverse function of digitToInt. It takes an Int in the range of 0..15
    and converts it to a lower-case character.

-}

-- The Caesar cipher is a primitive method of encoding messages by shifting each character
-- in them by a fixed number of positions in the alphabet.
import Data.Char as DC
encodeCaesarCipher :: Int -> String -> String
encodeCaesarCipher shift msg = map encodeChar msg
                                where
                                    encodeChar = DC.chr . (+ shift) . DC.ord

decodeCaesarCipher :: Int -> String -> String
decodeCaesarCipher shift msg = map decodeChar msg
                                where
                                    decodeChar = DC.chr . (subtract shift) . DC.ord

testCaesarCipher =  let testString = "Hello"
                    in (decodeCaesarCipher 3) . (encodeCaesarCipher 3) $ testString
