{- CIS 194 HW 11
   due Monday, 8 April

Files you should submit: SExpr.hs. You should take the version
that we have provided and add your solutions. Note that we have
also provided AParser.hs—you are welcome to use your own
AParser.hs from last week’s homework or ours, whichever you
prefer.

Remember, for this week’s homework you should only need to
write code on top of the interface provided by the Functor, Applicative,
and Alternative instances. In particular, you should not write any
code that depends on the details of the Parser implementation. (To
help with this, the version of AParser.hs we provided this week does
not even export the Parser constructor, so it is literally impossible to
depend on the details!)

-}

module SExpr where

import AParser
import Control.Applicative
import Data.Char

------------------------------------------------------------
--  1. Parsing repetitions
-- let’s see how to take a parser for (say) widgets and turn it
-- into a parser for lists of widgets.
------------------------------------------------------------

-- zeroOrMore takes a parser as input and runs it consecutively
-- as many times as possible (which could be none, if it fails
-- right away), returning a list of the results. zeroOrMore always
-- succeeds.
zeroOrMore :: Parser a -> Parser [a]
zeroOrMore p = oneOrMore p <|> pure []

-- oneOrMore takes a parser as input and runs it consecutively
-- as many times as possible
-- it requires the input parser to succeed at least once. If the
-- input parser fails right away then oneOrMore also fails
oneOrMore :: Parser a -> Parser [a]
oneOrMore p = liftA2 (:) p (zeroOrMore p)

-- tests
test_DiffBetweenOneOrMoreAndZeroOrMore = test1 && test2
    where
        test1 = Nothing == runParser (oneOrMore (satisfy isUpper)) "abcdeFGh"
        test2 = Just ("","abcdeFGh") == runParser (zeroOrMore (satisfy isUpper)) "abcdeFGh"
test_1 = test1 && test2
    where
        result = Just ("ABC","dEfgH")
        usingZeroOrMore = runParser (zeroOrMore (satisfy isUpper)) "ABCdEfgH"
        usingOneOrMore = runParser (oneOrMore (satisfy isUpper)) "ABCdEfgH"
        test1 = usingZeroOrMore == result
        test2 = usingOneOrMore == result

------------------------------------------------------------
--  2. Utilities
------------------------------------------------------------

spaces :: Parser String
spaces = zeroOrMore (satisfy isSpace)

ident :: Parser String
ident = liftA2 (:) readOneAlpha readZeroOrMoreAlphaNums
            where
                readOneAlpha = satisfy isAlpha
                readZeroOrMoreAlphaNums = zeroOrMore (satisfy isAlphaNum)

test_spaces_Nothing = Just ("","abcdeFGh") ==  runParser spaces "abcdeFGh"
test_spaces = Just ("          ","abcdeFGh") == runParser spaces "          abcdeFGh"

test_ident_Nothing = test1 && test2
    where
        test1 = Nothing == runParser ident "2bad"
        test2 = Nothing == runParser ident ""
test_ident_just = test1 && test2
    where
        test1 = Just ("foobar"," baz")  == runParser ident "foobar baz"
        test2 = Just ("foo33fA","")     == runParser ident "foo33fA"

------------------------------------------------------------
--  3. Parsing S-expressions
------------------------------------------------------------
-- S-expressions are a simple syntactic format for tree-structured data,
-- originally developed as a syntax for Lisp programs

-- An "identifier" is represented as just a String; however, only
-- those Strings consisting of a letter followed by any number of
-- letters and digits are valid identifiers.
type Ident = String

-- An "atom" is either an integer value or an identifier.
-- atom ::= int | ident
data Atom = N Integer | I Ident
  deriving Show

--
-- An S-expression is either an atom, or a list of S-expressions.
-- S ::= atom | (S*)
data SExpr = A Atom
           | Comb [SExpr]
  deriving Show
--  Actually, this is slightly different than the usual definition of
-- S-expressions in Lisp, which also includes binary “cons” cells;
-- but it’s good enough for our purposes.

valid_se_1 = "5"
valid_se_2 = "foo3"
valid_se_3 = "(bar (foo) 3 5 874)"
valid_se_4 = "(((lambda x (lambda y (plus x y))) 3) 5)"
valid_se_5 = "( lots of ( spaces in ) this ( one ) )"

{-
Hints: To parse something but ignore its output, you can use the
(*>) and (<*) operators, which have the types
(*>) :: Applicative f => f a -> f b -> f b
(<*) :: Applicative f => f a -> f b -> f a
p1 *> p2 runs p1 and p2 in sequence, but ignores the result of
p1 and just returns the result of p2. p1 <* p2 also runs p1 and p2 in
sequence, but returns the result of p1 (ignoring p2’s result) instead.
For example:
*AParser> runParser (spaces *> posInt) " 345"
Just (345,"")
-}

parseAtom :: Parser Atom
parseAtom = liftA N posInt <|> liftA I ident
-- OR parseAtom = N <$> posInt <|> I <$> ident

parseSExpr :: Parser SExpr
parseSExpr = atomParser <|> combParser
                where
                    atomParser  = liftA A (spaces *> parseAtom)
                    leftBracket = spaces *> char '('
                    rightBracket = spaces *> char ')'
                    sexprParser = leftBracket *> zeroOrMore parseSExpr <* rightBracket
                    combParser = liftA Comb sexprParser
