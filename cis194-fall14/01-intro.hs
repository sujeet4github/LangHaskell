{-
Haskell Basics
==============

CIS 194 Week 1\
28 August 2014
(http://www.seas.upenn.edu/~cis194/fall14/lectures/01-intro.html)

Suggested reading:
-   [Learn You a Haskell for Great Good, chapter 2]
    (http://learnyouahaskell.com/starting-out)
-   [Real World Haskell]
    (http://book.realworldhaskell.org/),
    chapters 1 and 2

Haskell is:

**Functional**

**Pure**
Haskell expressions are always *referentially transparent*, that is:

-   No mutation! Everything (variables, data structures) is
    *immutable*.

-   Expressions never have side effectsù (like updating global
    variables or printing to the screen).

-   Calling the same function with the same arguments results in the
    same output every time.

Benefits

-   *Equational reasoning and refactoring*

-   *Parallelism*: Evaluating expressions in parallel is easy when they
    are guaranteed not to affect one another.

-   *Fewer headaches*: Simply put, unrestricted effects and
    action-at-a-distance makes for programs that are hard to debug,
    maintain, and reason about.

**Lazy**

In Haskell, expressions are *not evaluated until their results are
actually needed*. This is a simple decision with far-reaching
consequences, which we will explore throughout the semester. Some of the
consequences include:

-   It is easy to define a new *control structure* just by defining a
    function.

-   It is possible to define and work with *infinite data structures*.

-   It enables a more compositional programming style (see *wholemeal
    programming* below).

-   One major downside, however, is that reasoning about time and space
    usage becomes much more complicated!

**Statically typed**

Every Haskell expression has a type, and types are all checked at
*compile-time*. Programs with type errors will not even compile, much
less run.

Themes
======
Throughout this course, we will focus on three main themes.

**Types**

Static type systems can seem annoying. In fact, in languages like C++ and Java,
they are annoying. But this isnít because static type systems per se are annoying;
itís because C++ and Javaís type systems are insufficiently expressive!

This semester weíll take a close look at Haskellís type system, which

- *Helps clarify thinking and express program structure*

    The first step in writing a Haskell program is usually to write down all
    the types. Because Haskellís type system is so expressive, this is a
    non-trivial design step and is an immense help in clarifying oneís thinking
    about the program.

- *Serves as a form of documentation*

    Given an expressive type system, just looking at a functionís type tells you
    a lot about what the function might do and how it can be used, even before you
    have read a single word of written documentation.

- *Turns run-time errors into compile-time errors*

    Itís much better to be able to fix errors up front than to just test a lot
    and hope for the best. "If it compiles, it must be correct" is mostly facetious
    (itís still quite possible to have errors in logic even in a type-correct
    program), but it happens in Haskell much more than in other languages.

**Abstraction**

"Donít Repeat Yourself" is a mantra often heard in the world of programming. Also
known as the "Abstraction Principle", the idea is that nothing should be duplicated:
every idea, algorithm, and piece of data should occur exactly once in your code.
Taking similar pieces of code and factoring out their commonality is known as the
process of abstraction.

Haskell is very good at abstraction: features like parametric polymorphism,
higher-order functions, and type classes all aid in the fight against repetition.
Our journey through Haskell this semester will in large part be a journey from
the specific to the abstract.

**Wholemeal programming**

Another theme we will explore is wholemeal programming. A quote from Ralf Hinze:

"Functional languages excel at wholemeal programming, a term coined by Geraint Jones.
Wholemeal programming means to think big: work with an entire list, rather than a
sequence of elements; develop a solution space, rather than an individual solution;
imagine a graph, rather than a single path. The wholemeal approach often offers
new insights or provides new perspectives on a given problem. It is nicely
complemented by the idea of projective programming: first solve a more general
problem, then extract the interesting bits and pieces by transforming the general
program into more specialised ones."

For example, consider this pseudocode in a C/Java-ish sort of language:

    int acc = 0;
    for ( int i = 0; i < lst.length; i++ ) {
      acc = acc + 3 * lst[i];
    }

This code suffers from what Richard Bird refers to as "indexitis": it has to worry
about the low-level details of iterating over an array by keeping track of a current
index. It also mixes together what can more usefully be thought of as two separate
operations: multiplying every item in a list by 3, and summing the results.

In Haskell, we can just write

    sum (map (3*) lst)

This semester weíll explore the shift in thinking represented by this way of
programming, and examine how and why Haskell makes it possible.


**Donít be scared of error messages!**

GHCís error messages can be rather long and (seemingly) scary. However, usually
theyíre long not because they are obscure, but because they contain a lot of useful
information! Hereís an example:

Prelude> 'x' ++ "foo"

<interactive>:1:1:
    Couldn't match expected type `[a0]' with actual type `Char'
    In the first argument of `(++)', namely 'x'
    In the expression: 'x' ++ "foo"
    In an equation for `it': it = 'x' ++ "foo"
First we are told ìCouldnít match expected type [a0] with actual type Charî. This
means that something was expected to have a list type, but actually had type Char.
What something? The next line tells us: itís the first argument of (++) which is
at fault, namely, 'x'. The next lines go on to give us a bit more context.
Now we can see what the problem is: clearly 'x' has type Char, as the first line
said. Why would it be expected to have a list type? Well, because it is used as
an argument to (++), which takes a list as its first argument.

When you get a huge error message, resist your initial impulse to run away; take
a deep breath; and read it carefully. You wonít necessarily understand the entire
thing, but you will probably learn a lot, and you may just get enough information
to figure out what the problem is.


Haskell Layout
==============
Haskell is a whitespace-sensitive language. This is in stark contrast
to most other languages, where whitespace serves only to separate
identifiers. (Haskell shares this trait with Python, which is also
whitespace-sensitive.) Haskell uses indentation level to tell where
certain regions of code end, and where new statements appear.

The basic idea is that, when a so-called layout herald appears, GHC
looks at the next thing it sees and remembers its indentation level.
A later line that begins at the exact same indentation level is
considered another member of the group, and a later line that begins
at a lesser (more to the left) indentation level is not part of the
group.

The layout heralds are where, let, do, of, and \ case.
Because Haskell modules begin with module Name where, that means that
the layout rule is in effect over the declarations in the file.

This means that the following is no good:
x :: Int
x =
5

The problem is that the 5 is at the same indentation level (zero)
as other top-level declarations, and so GHC considers it to be a new
declaration instead of part of the definition of x.

The layout rule is often a source of confusion for newcomers to
Haskell. But, if you get stuck, return to this decription (or, any of
the many online) and re-read ó often, if you think carefully enough
about it, youíll see whatís going on.

When calculating indentation level, tabs in code (you donít have
any of these, do you?!?) are considered with tab stops 8 characters
apart, regardless of what your editor might show you. This potential
confusion is why tabs are a terrible, terrible idea in Haskell code.

-}