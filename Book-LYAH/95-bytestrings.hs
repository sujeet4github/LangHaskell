-- http://learnyouahaskell.com/input-and-output#bytestrings
--

{-
Whenever you need better performance in a program that reads a lot of data into strings,
give bytestrings a try, chances are you'll get some good performance boosts with very
little effort on your part. I usually write programs by using normal strings and then
convert them to use bytestrings if the performance is not satisfactory.

import qualified Data.ByteString.Lazy as LazyByteStrings
import qualified Data.ByteString as StrictByteStrings

pack :: [Word8] -> ByteString
unpack is the inverse function of pack. It takes a bytestring and turns it into a list of bytes

fromChunks takes a list of strict bytestrings and converts it to a lazy bytestring.
toChunks takes a lazy bytestring and converts it to a list of strict ones.

cons - bytestring version of :
cons' - strict version of cons

empty makes empty bytestring

the bytestring modules have a load of functions that are analogous to those in Data.List,
including, but not limited to, head, tail, init, null, length, map, reverse, foldl, foldr,
concat, takeWhile, filter, etc.

It also has functions that have the same name and behave the same as some functions found
in System.IO, only Strings are replaced with ByteStrings.

-}
