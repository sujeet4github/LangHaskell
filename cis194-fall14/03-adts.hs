{-
http://www.seas.upenn.edu/~cis194/fall14/lectures/03-ADTs.html

CIS 194 Week 3 11 September 2014

Suggested reading:
Real World Haskell, chapters 2 - 4.

Libraries
=========
Haskell comes equipped with a decent standard library that can be accessed from any Haskell program.
Haskell libraries are distributed in packages, each of which can contain any number of modules.
A vanilla Haskell installation comes with the base package, among a few others. The base package
contains the Prelude module, which contains definitions that are automatically available in any
Haskell program. The base package also contains many other useful modules, which can be imported
with a statement like this:

import Data.Char ( toUpper )
That line imports the Data.Char module, but only grabs the definition for toUpper, as we’ll use
below. The parenthesized bit is optional; if it is left out, all definitions from the imported
module are brought in. You can read documentation for the base package on [Hackage][] – search
for base. Of particular use toward the beginning of the course is the Data.List module.

Haskell also provides a facility to (somewhat) easily download and install new packages to use.
[Hackage][] is the main distribution server for these packages, and cabal is a program, installed
with the Haskell Platform, that pulls packages down from this server and installs them. Here is
how you do it at a command prompt:

~> cabal update
~> cabal install text-1.1.1.3

The first line instructs cabal to download an updated list of available packages; the second line
installs version 1.1.1.3 of the text package. You can leave the version number out; that will
install the most recent version. The text package provides a way to store chunks of text (strings,
essentially) that is considerably more efficient than String. These instructions tell you to use
version 1.1.1.3 because the most recent version, 1.2.0.0, is not compatible with some other packages
you might want to install later on.

Enumerations
============
-}
data Thing = Shoe
           | Ship
           | SealingWax
           | Cabbage
           | King
  deriving Show

{-
Beyond Enumerations - ADT
=========================

enumerations are actually only a special case of Haskell’s more general algebraic data types.

-}
data FailableDouble =   Failure
                        | OK Double
    deriving Show
ex01 = Failure
ex02 = OK 3.4
safeDiv :: Double -> Double -> FailableDouble
safeDiv _ 0 = Failure
safeDiv x y = OK (x/y)

{-
Algebraic data types in general
===============================

In general, an algebraic data type has one or more data constructors, and each data constructor
can have zero or more arguments.

data AlgDataType = Constr1 Type11 Type12
                 | Constr2 Type21
                 | Constr3 Type31 Type32 Type33
                 | Constr4
This specifies that a value of type AlgDataType can be constructed in one of four ways:
using Constr1, Constr2, Constr3, or Constr4.

Depending on the constructor used, an AlgDataType value may contain some other values.
For example, if it was constructed using Constr1, then it comes along with two values,
one of type Type11 and one of type Type12.

type and data constructor names must always start with a capital letter;
variables (including names of functions) must always start with a lowercase letter.
(Otherwise, Haskell parsers would have quite a difficult job figuring out which names
represent variables and which represent constructors).

data Person = Person String Int Thing
  deriving Show

Idiomatically the type constructor and data constructor are usually named the same
(if there is only one data constructor), but they inhabit different namespaces and
are different things.
This idiom (giving the type and data constructor of a one-constructor type the same
name) is common, but can be confusing until you get used to it.

-}


{-
Pattern Matching
================

Fundamentally, pattern-matching is about taking apart a value by finding out which
constructor it was built with. This information can be used as the basis for deciding
what to do—indeed, in Haskell, this is the only way to make a decision.

In general, the following grammar defines what can be used as a pattern:
pattern ::= _
            |  var
            |  var @ ( pat )
            |  ( Constructor pat1 pat2 ... patn )
(In actual fact, the full grammar of patterns includes yet more features still, but
the rest would take us too far afield for now.)

Case Expressions
================
The fundamental construct for doing pattern-matching in Haskell is the case expression.
In general, a case expression looks like

case exp of
  pat1 -> exp1
  pat2 -> exp2
  ...


Recursive Data Types
====================
data IntList = Empty
                | Cons Int IntList

Functions as First class Objects
================================
a function – an operation that can be run – is considered a chunk of data just like any
other chunk of data. Functions can be stored in variables and passed to other functions
just like any other data. Just like the type of a character is written as Char, the type
of a function is written with an arrow, ->. The difference is that a function type must
also specify the domain of the function (what type of argument it takes) and the codomain
of the function (what type of value it produces).

-}