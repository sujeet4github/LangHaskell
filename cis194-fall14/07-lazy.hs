{-
http://www.seas.upenn.edu/~cis194/fall14/lectures/07-laziness.html

CIS 194 Week 7 16 October 2014

Suggested reading:
Real World Haskell, Chapter 25: Profiling and Optimization

The benefit of strict evaluation is that it is easy to predict when and
in what order things will happen. Usually languages with strict evaluation
will even specify the order in which function arguments should be
evaluated (e.g. from left to right).

Side effects and purity
=======================
So, what’s really at issue here is the presence or absence of side effects.
By “side effect” we mean anything that causes evaluation of an expression
to interact with something outside itself.
The root issue is that such outside interactions are time-sensitive.

For example:
Modifying a global variable: it matters when this happens since it may
    affect the evaluation of other expressions
Printing to the screen: it matters when this happens since it may need
    to be in a certain order with respect to other writes to the screen
Reading from a file or the network: it matters when this happens since
    the contents of the file can affect the outcome of the expression

As we have seen, lazy evaluation makes it hard to reason about when things
will be evaluated; hence including side effects in a lazy language would
be extremely unintuitive. Historically, this is the reason Haskell is pure:
initially, the designers of Haskell wanted to make a lazy functional language,
and quickly realized it would be impossible unless it also disallowed side effects.

But… a language with no side effects would not be very useful. The only thing
you could do with such a language would be to load up your programs in an
interpreter and evaluate expressions. (Hmm… that sounds familiar…)

You would not be able to get any input from the user, or print anything to the
screen, or read from a file. The challenge facing the Haskell designers was
to come up with a way to allow such effects in a principled, restricted way
that did not interfere with the essential purity of the language.
They finally did come up with something (namely, the IO monad) which we’ll talk about in a few weeks.

Lazy evaluation
===============
So now that we understand strict evaluation, let’s see what lazy evaluation
actually looks like. Under a lazy evaluation strategy, evaluation of function
arguments is delayed as long as possible: they are not evaluated until it
actually becomes necessary to do so.

When some expression is given as an argument to a function, it is simply
packaged up as an unevaluated expression (called a “thunk”, don’t ask me why)
without doing any actual work.

Notes:
Pattern Matching Drives Evaluation
* Expressions are only evaluated when pattern-matched
* - only as far as necessary for the match to proceed, and no farther!

Consequences of Lazy Evaluation:
1. choosing a lazy evaluation strategy essentially forces you to also choose purity
2. infinite data structures
3. pipelining / wholemeal programming
4. One of the downsides is that it sometimes becomes tricky to reason about the space
    usage of your programs.
5. avoiding laziness - use functions: seq or BangPatterns language extension

Profiling
=========
To profile a program, it will first need a main – the actions taken by main are what is profiled.
Then, compile it with the -rtsopts option, which enables extra arguments to be passed into your
built program. When running a program built with -rtsopts, you can pass the +RTS option;
every option you pass after that goes straight to the runtime system, not your program.
These options can control profiling. We’ll be using three such options:
    -s makes your program dump out memory and time usage information when it is done running.
    -h makes your program produce a heap profile.
        A heap profile details how much memory your program takes up over time.
        This profile is left in the file YourProgram.hp where your source file
        is named YourProgram.hs and your compiled program is named YourProgram
        (or YourProgram.exe, on Windows).
    -i allows you to set the heap profiling interval, that is, how often the
        heap is profiled. If your heap profile is empty, you may need to shorten
        the interval. The -i is followed by a the number of seconds (it is often
        a decimal) between heap checks. I’ve had success with -i0.001.
        The default is 0.1, which is too long for the small programs we’ll be testing.

After a heap profile is created, it needs to be converted into a viewable format. This is done
with the hp2ps utility. That separate program takes many options, but we’ll always use -c,
which enables color. You call hp2ps on your .hp file, and it creates a .ps file, which can
be opened in just about any PDF viewer.

Let’s put this all into practice. I want a heap profile of lazySum called on the numbers 0 through 1,000,000:

-}
lazySum :: Num a => [a] -> a
lazySum = go 0
  where go acc []     = acc
        go acc (x:xs) = go (x + acc) xs

main = print (lazySum [1..10000000])

{- Command Line:
ghc Lec07.hs -rtsopts
Lec07.exe +RTS -s -h -i0.001
hp2ps -c Lec07.hp

The GHC Manual details the runtime system options and profiling in good detail.

-}

