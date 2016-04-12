{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module HW06 where

{-
You will need two packages that are not part of Haskell’s standard
library for this assignment. They are aeson and text. You can install
these with cabal update; cabal install aeson text

The markets.json file you have downloaded contains information
about many (all?) of the Farmers’ Markets that regularly take
place throughout the USA, originally retrieved via
http://catalog.data.gov/dataset/farmers-markets-geographic-data.
That website produces the data in an Excel spreadsheet.
I converted the spreadsheet to a comma-separated-values format (CSV)
and then used http://www.convertcsv.com/csv-to-json.htm to get it
into a JSON format. I chose JSON because the aeson JSON parser
is more advanced yet easier to use than the CSV parser package, cassava.

-}

import Data.Aeson
import Data.Monoid
import GHC.Generics
import Data.List {- for sort -}

{-
The reason to derive Generic is for easy interoperability with the aeson
JSON-parsing library. A derived Generic instance encodes various
features of a datatype (such as its constructor names, any record-field
names, etc.) that advanced Haskellers can (such as the authors of
aeson) use to make your life easier
-}

-- why qualifying - explanation given below
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Text as T
import qualified Data.Text.IO as T

{-
This homework centers around parsing and then querying information
stored in a JSON file.

What you do have to worry about is making sure that your Haskell
program can find your markets.json file. Putting the file in the same
directory as your HW06.hs file is a great start, but it’s not always
enough. If you’re having trouble getting your code to find your file,
and you’re using GHCi, try running :!pwd. That will print out the
current directory GHCi thinks it’s in. (The :! prefix allows you to
run arbitrary shell commands within GHCi.) If markets.json isn’t
there, either move it there, or use :cd to move GHC

Strings
-------
1. Built in - string - list of characters
2. ByteString
    The ByteString library helpfully (?) uses many of the same names
    for functions as the Prelude and Data.List. If you just import Data.ByteString,
    you’ll get a ton of name clashes in your code. Instead, we use import qualified ... as B,
    which means that every use of a ByteString function (including operators)
    or type must be preceded by B.. Thus, to get the length of a
    ByteString, you use B.length. Even to mention the type ByteString,
    you must use B.ByteString.
    ByteStrings come in several flavors, depending on whether they
    are lazy or strict and what encoding they use internally. Laziness is a
    story for another day, and we really don’t want to worry about encodings.
    For now, use Data.ByteString.Lazy.Char8 and everything will
    work out nicely.
3. Text
    Text is quite like ByteString: it also reuses a lot of familiar names.
    It also comes in two laziness flavors. We’ll be using the strict flavor,
    which is provided in Data.Text. You also may want some I/O operations,
    so the import statements above include the Data.Text.IO
    module.

When working with non-String strings, it is still very handy to
use the "..." syntax for writing Text or ByteString values. So,
GHC provides the OverloadedStrings extension.

A consequence of OverloadedStrings is that sometimes GHC
doesn’t know what string-like type you want, so you may need to
provide a type signature. You generally won’t need to worry about
OverloadedStrings as you write your code for this assignment, but
this explanation is meant to help if you get strange error messages.

-}

{-
One aeson function that parses JSON is called eitherDecode:
eitherDecode :: FromJSON a => ByteString -> Either String a

-}


{-
Exercise 1 Write a function

ynToBool :: Value -> Value
that changes all occurrences of String "Y" to be
Bool True and all occurrences of String "N" to be
Bool False.
No other part of the input Value should change.
-}
ynToBool :: Value -> Value
ynToBool (String "Y") = Bool True
ynToBool (String "N") = Bool False
ynToBool (Array a)  = Array (fmap ynToBool a)
ynToBool (Object o) = Object (fmap ynToBool o)
ynToBool x = x

{-
Exercise 2 Write a function

parseData :: B.ByteString -> Either String Value
that takes in a ByteString containing JSON data and outputs either
an error message or a Value that has been processed by ynToBool.

Hint: This can be very short, if you use Either’s Functor instance!
You can easily test this in ghci:

*HW06> filedata <- B.readFile "markets.json"
*HW06> parseData filedata
-}
parseData :: B.ByteString -> Either String Value
parseData = fmap ynToBool . eitherDecode

{-
Exercise 3:
Write a Market type, including the fields that interest
you. At a minimum, include marketname, x (the longitude of the
market), y (the latitude of the market), and state. Use T.Text to
represent text. (String also works, but is less efficient.)
-}
data Market = Market { fmid       :: Int
                     , marketname :: T.Text
                     ,  x         :: Double
                     ,  y         :: Double
                     ,  state     :: T.Text
                     } deriving (Show, Eq, Generic)
--
-- Thanks to deriving Generic we get our type becoming automatic instances of
-- FromJSON and ToJSON and therefore get automatic parsers for this.
-- e.g eitherDecode, decode, encode
--
-- *HW06> eitherDecode filedata :: (Either String [Market])

instance FromJSON Market

{-
Exercise 3:
-}
parseMarkets :: B.ByteString -> Either String [Market]
parseMarkets byteStringData = parsedFilteredData >>= transformResultToEither . fromJSON
    where
        parsedFilteredData = parseData byteStringData
        transformResultToEither (Success a) = Right a
        transformResultToEither (Error e) = Left e
-- Earlier Version
parseMarkets2 byteStringData =
    -- fmap fromJSON $ parseData filedata :: (Either String (Result [Market]))
    -- how to move from (Either String (Result [Market])) to (Either String [Market]) ?
    case parseData byteStringData of
        (Left e)    -> Left e
        (Right d)   -> jsonParse d
    where
        jsonParse d = case fromJSON d of
            (Success s) -> (Right s)
            (Error e)   -> (Left e)

inputFile :: FilePath
inputFile = "markets.json" -- "markets_less.json"

{-
Exercise 4 Write an I/O action
that loads the market data. In the event of a parsing failure, report
the error using fail :: String -> IO a. (fail aborts an action,
reporting an error to the user. It never returns, so it can be used no
matter what IO type is expected.
-}
loadData :: IO [Market]
loadData = do
    rawJsonData <- B.readFile inputFile
    case (parseMarkets rawJsonData) of
        Left msg        -> fail msg
        Right markets   -> return markets

-- Testing:
-- *HW06> mkts <- loadData

{-
Interlude: an ordered-list monoid
Exercise 5 Write the OrdList datatype and its Monoid instance.
-}
data OrdList a = OrdList { getOrdList :: [a] }
                    deriving (Eq, Show)

instance Ord a => Monoid (OrdList a) where
    mempty = OrdList []
    mappend (OrdList xs) (OrdList ys) = OrdList (sort $ xs ++ ys)

evens :: OrdList Integer
evens = OrdList [2,4,6]
odds :: OrdList Integer
odds = OrdList [1,3,5]
combined :: OrdList Integer
combined = evens <> odds
-- combined should have the value OrdList [1,2,3,4,5,6],
-- because the (<>) operator maintains the ordering invariant.

{-
Now that you have a way of loading the market data, you need a way
of searching through that data. Furthermore, it would be nice if the
search mechanism is flexible enough to produce data in a Monoid of
the caller’s choice.

Here, all of our queries are going to center on searching
for a market’s name (the marketname field).

Throughout this section, we will be searching among the markets
returning a variety of types. To avoid code repetition, it is helpful to
use a type synonym. Include the following in your code:
-}
-- a Searcher m is a function that, when given the T.Text to
-- search for in a [Market], will produce an m.
type Searcher m = T.Text -> [Market] -> m

{-
Exercise 6
Write a function that searches through the provided list of Markets for market names
containing the given T.Text (Data.Text.isInfixOf will be useful here).

With each found market record, use the function provided to
convert the market record into the monoid of the caller’s choice, and
then combine all the individual results using mappend and mconcat.

Note that we can always expand type synonyms in Haskell. So,
the type of search is fully equivalent to Monoid m => (Market -> m) -> T.Text -> [Market] -> m.

This means that the definition for search may include up to three arguments,
even though the type looks like it should take only one.

Hint: This function should not be very long. If it’s getting long,
you’re probably doing something the wrong way. You may also want
to check out the intInts example from the lecture notes.

Hint: Using an as-pattern may be helpful.
Here is an example:
marketWithName :: Market -> (T.Text, Market)
marketWithName mkt@(Market { marketname = name }) = (name, mkt)
-}

search :: Monoid m => (Market -> m) -> Searcher m
search marketToMonoidMapper snippet markets = mconcat filteredMonoids
    where
        filteredMonoids = map marketToMonoidMapper filteredMarkets
        filteredMarkets = filter (T.isInfixOf snippet . marketname) markets

{-
Exercise 7
Write a function that returns the first market found by a search, if any are found at all.
Hint: The following function may be useful for all the searching
exercises. Look at the type to figure out what it does:
-}
compose2 :: (c -> d) -> (a -> b -> c) -> a -> b -> d
compose2 = (.) . (.)

firstFound :: Searcher (Maybe Market)
firstFound = getFirst `compose2` search (\m -> (First (Just m)))

lastFound :: Searcher (Maybe Market)
lastFound = getLast `compose2` search (\m -> (Last (Just m)))

allFound :: Searcher [Market]
allFound = search (:[])

numberFound :: Searcher Int
numberFound = getSum `compose2` search (\m -> Sum 1)

-- use T.pack "hello" to convert from char to text

instance Ord Market
    where
        compare l r = compare (y l) (y r)

newtype OrdMarkets = OrdMarkets { getOrdMarkets :: [Market] }
instance Monoid OrdMarkets where
        mempty = OrdMarkets []
        mappend (OrdMarkets x) (OrdMarkets y)
            = OrdMarkets $ sort $ x <> y

orderedNtoS :: Searcher [Market]
orderedNtoS = getOrdMarkets `compose2` search (\m -> OrdMarkets [m])

test :: Searcher a -> String -> IO a
test f t = loadData >>= return . (f (T.pack t))
