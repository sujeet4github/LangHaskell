{- CIS 194 HW 10
   http://www.seas.upenn.edu/~cis194/spring13/hw/10-applicative.pdf
-}

module AParser where

import           Control.Applicative

import           Data.Char

{-
A parser is an algorithm which takes unstructured data as input (often
a String) and produces structured data as output. For example,
when you load a Haskell file into ghci, the first thing it does is parse
your file in order to turn it from a long String into an abstract syntax
tree representing your code in a more structured form.
-}
-- Unstructured Data --> |Parser| --> Structured Data

-- A parser for a value of type a is a function which takes a String
-- represnting the input to be parsed, and succeeds or fails; if it
-- succeeds, it returns the parsed value along with the remainder of
-- the input.
newtype Parser a = Parser { runParser :: String -> Maybe (a, String) }

-- For example, 'satisfy' takes a predicate on Char, and constructs a
-- parser which succeeds only if it sees a Char that satisfies the
-- predicate (which it then returns).  If it encounters a Char that
-- does not satisfy the predicate (or an empty input), it fails.
satisfy :: (Char -> Bool) -> Parser Char
satisfy test = Parser f
  where
    f [] = Nothing    -- fail on the empty input
    f (x:xs)          -- check if x satisfies the predicate
                        -- if so, return x along with the remainder
                        -- of the input (that is, xs)
        | test x       = Just (x, xs)
        | otherwise = Nothing  -- otherwise, fail

-- Using satisfy, we can define the parser 'char c' which expects to
-- see exactly the character c, and fails otherwise.
char :: Char -> Parser Char
char c = satisfy (== c)

{- For example:

*Parser> runParser (satisfy isUpper) "ABC"
Just ('A',"BC")
*Parser> runParser (satisfy isUpper) "abc"
Nothing
*Parser> runParser (char 'x') "xyz"
Just ('x',"yz")

-}

-- For convenience, we've also provided a parser for positive
-- integers.
posInt :: Parser Integer
posInt = Parser f
  where
    f xs
      | null ns   = Nothing
      | otherwise = Just (read ns, rest)
      where (ns, rest) = span isDigit xs

------------------------------------------------------------
-- Your code goes below here
------------------------------------------------------------

-- Building blocks for testing {{
type Name = String
type PhoneNumber = String
data Employee = Employee { name :: Name,
                           phone :: PhoneNumber}
                | EmployeeNoPhone { name :: Name }
                           deriving (Show, Eq)
parseName :: Parser Name
parseName = Parser f where
                      f xs
                        | null ns   = Nothing
                        | otherwise = Just (ns, rest)
                        where (ns, rest) = span isLetter xs
test_parseName_OkLetters =
                    let lhs = runParser parseName "Hello:123 xyz"
                        rhs = Just ("Hello",":123 xyz")
                    in lhs == rhs
test_parseName_OkNonLetters = rhs == lhs
                        where
                            lhs = runParser parseName "1Hello:123 xyz"
                            rhs = Nothing

parsePhone :: Parser PhoneNumber
parsePhone = Parser f where
                      f xs
                        | null ns   = Nothing
                        | otherwise = Just (ns, rest)
                        where (ns, rest) = span isDigit xs
-- Building blocks for testing End }}


{-
However, implementing parsers explicitly like this is tedious and
error-prone for anything beyond the most basic primitive parsers.
The real power of this approach comes from the ability to create complex
parsers by combining simpler ones. And this power of combining
will be given to us by. . . you guessed it, Applicative.
-}
-- Exercise 1
-- First, you will need to implement a Functor instance for Parser.
-- Hint: You may find it useful to implement a function
-- first :: (a -> b) -> (a,c) -> (b,c)
  -- apply the function to the first of the tuple
first :: (a -> b) -> (a,c) -> (b,c)
first f (res, remStr) = (f res, remStr)
test_first = first (+100) (1, 2) == (101, 2)

instance Functor Parser where
  -- map a function (e.g. data constructor like EmployeeNoPhone or Employee)
  -- over a Parser
  fmap f p = Parser (\string ->
                let parsingResult = runParser p string
                -- parsing result,
                --  if failed is Nothing
                --  if good, is something like Just(token-retrieved-by-parser-p, rest-of-string)
                -- We need to get to:
                --  Just (f token-retrieved-by-parser-p, rest-of-string)
                -- the `fmap` is the fmap of the Maybe functor
                -- the function that is lifted up into the Maybe functor is
                -- f applied to first
                in fmap (first f) parsingResult)
{-
    Version 1:
  fmapV1 f p = Parser nf
              where
                nf x = case (runParser p x) of
                  Nothing -> Nothing
                  Just(a,b) -> Just(first f (a,b))

    Version 2: (after reading: http://okigiveup.net/2015/sep/7/small-adventure-functors/)
  fmapV2 f p = Parser (\string ->
                let parsingResult = runParser p string
                -- parsing result,
                --  if failed is Nothing
                --  if good, is something like Just(token-retrieved-by-parser-p, rest-of-string)
                -- We need to get to:
                --  Just (f token-retrieved-by-parser-p, rest-of-string)
                -- the `fmap` is the fmap of the Maybe functor
                -- the function that is lifted up into the Maybe functor is
                -- f applied to first
                in (first f) `fmap` parsingResult)
                -- OR
                -- in fmap (first f) parsingResult)

    The solution was included as a part of the homework for week 11, and it looked radically
    different from mine:
[RDS1]
        inParser f = Parser . f . runParser
[RDS2]
        instance Functor Parser where
          fmap = inParser . fmap . fmap . first

*AParser> :t inParser . fmap . fmap . first
inParser . fmap . fmap . first
  :: (a1 -> a) -> Parser a1 -> Parser a
1st argument: function (a1 -> a)
2nd argument: parser on a1
result      : parser on a  (result of 1st function)

How does it do this? {{{
inParser is composed of Parser . f . runParser
    where f = fmap . fmap . first

What does runParser do? {{
*AParser> :t runParser
runParser :: Parser a -> String -> Maybe (a, String)

takes a Parser and a string and applies parser to string to
get Maybe (<result-of-parser-parsing>, <remainder-string>)
}}

fmap . fmap . first is applied against result of runParser....

we ignore the first fmap and look at fmap . first
what does this do? {{

*AParser> :t fmap . first . runParser
fmap . first . runParser
  :: Functor f =>
     Parser a -> f (String, c) -> f (Maybe (a, String), c)

The first function applies the function-argument to the first of a pair.
This first function is 'lifted' into the Maybe which is a Functor

}}

The second fmap, lifts the current Maybe Functored entity futher up into
another Functor, which is the function composition Functor with fmap=(.)

}}}

An experienced Haskell developer, sees the commonality between manipulating
the result of a function, and what is inside a Maybe, and uses the same
interface for both.


Type Signatures:
first :: (a -> b) -> (a, c) -> (b, c)
fmap :: Functor f => (a -> b) -> f a -> f b

Remember in Haskell, every function can be considered 1 param function:
    a -> b -> c   is the same as a -> (b -> c)

How is fmap mapped on to first?
Remember:
(.) :: (b -> c) -> (a -> b) -> (b -> c)

first :: (a -> b) -> (a, c) -> (b, c)
OR
first :: (a -> b) -> ( (a, c) -> (b, c) )

in fmap . first, the output of first with is the first argument of fmap
fmap :: Functor f => (a -> b) -> f a -> f b

(.) :: (c -> b) -> (a -> c) -> a -> b
Input of (.)        : (c -> b)
Result of (.)       : (a -> c) -> a -> b

Input of fmap       : (x -> y)
Result of fmap      : f x -> f y

Input of (.) fmap   : p -> (x -> y)
applying fmap       : p -> (f x -> f y)
Result of (.) fmap  : p -> f x -> f y
(.) fmap :: (p -> (x -> y)) -> p -> f a -> f b

Input of first      : (a -> b)
Result of first     : (a, c) -> (b, c)

Input of (.) fmap first     : p -> (x -> y) -> (a -> b)
Result of (.) fmap first    = f (a, c) -> f (b, c)
(.) fmap first :: (a -> b) -> f (a, c) -> f (b, c)

fmap(first)     :: Functor f => (a -> b) -> f (a, c) -> f (b, c)
fmap . first    :: Functor f => (a -> b) -> f (a, c) -> f (b, c)

*AParser> :t fmap . first
(fmap . first)  :: Functor f => (a -> b) -> f (a, c) -> f (b, c)

(fmap . first)  :: Functor f => (a -> b) -> (f (a, c) -> f (b, c))

Now we do another fmap as in [RDS2]
fmap . fmap . first :: (Functor f, Functor f2) => ( f (a, c) -> f (b, c) ) -> f2 (f (a, c)) -> f2 (f (b, c))

*AParser> :t fmap . fmap . first
fmap . fmap . first
  :: (Functor f, Functor f1) => (a -> b) -> f (f1 (a, c)) -> f (f1 (b, c))
[RDS2-S2]

Now, we analyze inParser from [RDS1]
*AParser> :t inParser
inParser
  :: ((String -> Maybe (a1, String)) -> String -> Maybe (a, String))
     -> Parser a1 -> Parser a

inParser takes a Complicated Function, and a Parser and returns another Parser

Complicated Function has following type signature:
(String -> Maybe (a1, String)) -> String -> Maybe (a, String)
[RDS1-S1]

Complicated Function:
    1st arg: function String -> Mapbe (a1, String) , note a1 is what Parser 1 wraps
    2nd arg: String
    Returns: Maybe (a, String), note a is what the return Parser of inParser wraps

type of  runParser
*AParser> :t runParser
runParser :: Parser a -> String -> Maybe (a, String)

reorder Complicated Function type signature into:
(String -> Maybe (a1, String))
    -> (String -> Maybe (a, String))

can this match with the return type of fmap . fmap . first
from [RDS2-S2],
this is:
    f (f1 (a, c)) -> f (f1 (b, c))

so f1 can be Maybe Functor
what about f? - Note functions are themselves functors, with fmap of (.)!!!!

instance Functor ((->) r) where
    fmap = (.)

How to Verify this?
{{{
*AParser> :t inParser . fmap . fmap . first
inParser . fmap . fmap . first
  :: (a1 -> a) -> Parser a1 -> Parser a
*AParser> :t inParser . (.) . fmap . first
inParser . (.) . fmap . first :: (a1 -> a) -> Parser a1 -> Parser a

The resulting types match!!!
}}}

Now from [RDS2-S2]
    fmap . fmap . first
      :: (Functor f, Functor f1) => (a -> b) -> f (f1 (a, c)) -> f (f1 (b, c))
from [RDS1-S1]
    (String -> Maybe (a1, String)) -> String -> Maybe (a, String)

in [RDS2-S2]
    f is matched to (.)
    f1 is matched to Maybe (a1, String)

-}

testFunctorEmployeeParser = actual == expected
                    where
                      pf =  fmap EmployeeNoPhone parseName
                      actual =  runParser pf "Hello123"
                      expected = Just (EmployeeNoPhone {name = "Hello"},"123")

-- Exercise 2
-- Now implement an Applicative instance for Parser:
instance Applicative Parser where
  -- pure a represents the parser which consumes no input
  -- and successfully returns a result of a.
  pure a = Parser f
            where
              f x = Just (a, x)
  -- p1 <*> p2 represents the parser which first runs p1 (which will
  -- consume some input and produce a function), then passes the
  -- remaining input to p2 (which consumes more input and produces
  -- some value), then returns the result of applying the function
  -- to the value.
  -- However, if either p1 or p2 fails then the whole thing should
  -- also fail (put another way, p1 <*> p2 only succeeds if both p1 and
  -- p2 succeed).
  p1 <*> p2 = Parser f
                where
                  f x = case (runParser p1 x) of
                    Nothing -> Nothing
                    Just(rf, b) -> case (runParser p2 b) of
                          Nothing -> Nothing
                          Just (c, d) -> Just (rf c, d)
{-
  we could now use the Applicative instance for Parser to make an
  employee parser from name and phone parsers.

  Emp <$> parseName <*> parsePhone :: Parser Employee
  is a parser which first reads a name from the input, then a phone
  number, and returns them combined into an Employee record. Of
  course, this assumes that the name and phone number are right
  next to each other in the input, with no intervening separators. Weâ€™ll
  see later how to make parsers that can throw away extra stuff that
  doesnâ€™t directly correspond to information you want to parse.
-}
employeeParse = Employee <$> parseName <*> parsePhone
testEmployeeParse = actual == expected
                    where
                      actual =  runParser employeeParse  "Hello123"
                      expected = Just (Employee {name = "Hello", phone = "123"},"")

-- Exercise 3
{-
We can also test your Applicative instance using other simple
applications of functions to multiple parsers. You should implement
each of the following exercises using the Applicative interface to put
together simpler parsers into more complex ones. Do not implement
them using the low-level definition of a Parser! In other words, pretend
that you do not have access to the Parser constructor or even
know how the Parser type is defined.
-}

-- which expects to see the characters ’a’ and ’b’ and returns them
-- as a pair
abParserV1 = pure (,) <*> char 'a' <*> char 'b'
abParserV2 = (,) <$> char 'a' <*> char 'b'
abParserV3 = functor <*> char 'b'
            where
                functor = fmap (,) (char 'a')

abParser :: Parser (Char, Char)
abParser = abParserV1
-- (,) is kind of like a constructor for empty tuple
-- (,) :: a -> b -> (a, b)
-- e.g.  (,) 2 3
-- this behaves like the lambda: (\x y -> (x, y))
-- we push it into the Parser Applicative Context/Box, and then we
-- add in parsers to search for 'a' and 'b'
-- this creates a parser that we can use as below,
--
test_abParser_has_ab = Just (('a','b'),"cdef") == runParser abParser "abcdef"
test_abParser_no_ab = Nothing == runParser abParser "aebcdf"

--  acts in the same way as abParser but returns () instead of
-- the characters ’a’ and ’b’.
abParser_ :: Parser ()
abParser_ = abParser_V1
abParser_V1 = fmap (\x -> ()) abParser
abParser_V2 = (\_ _ -> ()) <$> char 'a' <*> char 'b'
abParser_V3 = functor <*> char 'b'
            where
                functor = fmap (\_ _ -> ()) (char 'a')

test_abParser__has_ab = Just ((),"cdef") == runParser abParser_ "abcdef"
test_abParser__no_ab = Nothing == runParser abParser_ "aebcdf"

-- reads two integer values separated by a space and
-- returns the integer values in a list.
-- use the provided posInt to parse the integer values.
intPair :: Parser [Integer]
intPair =   let
                twoIntsSepByCharParser = (,,) <$> posInt <*> (char ' ') <*> posInt
                convThreeTupleToList = (\(x,_,y) -> [x,y])
            in
                fmap convThreeTupleToList twoIntsSepByCharParser

test_intPair :: Bool
test_intPair = Just ([12,34],"") ==  runParser intPair "12 34"

-- Exercise 4
{-
Applicative by itself can be used to make parsers for simple, fixed
formats. But for any format involving choice (e.g. “. . . after the colon
there can be a number or a word or parentheses. . . ”) Applicative is
not quite enough. To handle choice we turn to the Alternative class,
-}
class Applicative f => Alternative f where
    -- empty should be the identity element for
    -- (<|>), and often represents failure.
    empty :: f a
    -- intended to represent choice: that is, f1 <|> f2 represents
    -- a choice between f1 and f2
    (<|>) :: f a -> f a -> f a

-- Write an Alternative instance for Parser:
instance AParser.Alternative Parser where
    f1 <|> f2 = Parser pf where
                  pf x = case (runParser f1 x) of
                    Nothing -> case (runParser f2 x) of
                          Nothing -> Nothing
                          something -> something
                    something -> something
    empty   = Parser (const Nothing)


-- Exercise 5
-- note returns ()
intOrUppercase :: Parser ()
intOrUppercase = lhs AParser.<|> rhs
                where
                    lhs = const () <$> posInt
                    rhs = const () <$> satisfy isUpper

test_intOrUppercase :: Bool
test_intOrUppercase = test1 && test2 && test3
                where
                    test1 = Just ((), "abcd")   == runParser intOrUppercase "342abcd"
                    test2 = Just ((), "YZ")     == runParser intOrUppercase "XYZ"
                    test3 = Nothing             == runParser intOrUppercase "foo"
