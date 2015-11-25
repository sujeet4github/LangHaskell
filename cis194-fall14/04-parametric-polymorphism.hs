{-
http://www.seas.upenn.edu/~cis194/fall14/lectures/04-poly.html

CIS 194 Week 4 18 September 2014

While completing previous homework assignments, you probably spent a fair
amount of time repeating yourself in certain parts of your code structure.
For example, wordsFrom and wordsFittingTemplate in HW2 were awfully similar
in structure. And the MaybeLogMessage and MaybeInt structures from HW3 are
very similar. Can we abstract these patterns away and avoid this repetition
in code? Sure we can!

Polymorphic data types
======================

Let’s take a look at (almost) two of the data structures from HW3:

data LogMessage = LogMessage Int String
data MaybeLogMessage = ValidLM LogMessage
                     | InvalidLM
data MaybeInt = ValidInt Int
              | InvalidInt
Those last two data structures are awfully similar. They both represent the
possibility of failure. That is, they both optionally hold some type a;
in the first, a is instantiated to LogMessage, and in 2nd to Int.
It turns out that we can write this more directly:

data Maybe a = Just a
             | Nothing

a is called a type variable – it’s a variable that stands in for a type
To become a proper, full-blooded type, we must supply Maybe with another
type, like LogMessage or Int.

-}


data List t = Empty | Cons t (List t) deriving Show
-- Given a type t, a (List t) consists of either the constructor Empty,
-- or the constructor Cons along with a value of type t and another
-- (List t). Here are some examples:

lst1 :: List Int
lst1 = Cons 3 (Cons 5 (Cons 2 Empty))

lst2 :: List Char
lst2 = Cons 'x' (Cons 'y' (Cons 'z' Empty))

lst3 :: List Bool
lst3 = Cons True (Cons False Empty)


{-
Polymorphic functions
=====================
It’s great that we can create polymorphic structures, but these become
even more useful when we operate over them polymorphically.

For example, let’s say we want retrive the first element of a list.

But, we don’t know whether that list has any elements at all, and we
need to be able to return something if we have an empty list.

Our return type will thus be a Maybe. The type parameter should be
left unspecified – that is, a:
-}
safeHead :: List a -> Maybe a
safeHead Empty      = Nothing
safeHead (Cons x _) = Just x
{-
Here, we have used a in a type signature in exactly the same way that
we did when defining the List and Maybe types. It simply stands in for
a type that will be specified later.

To figure out what type should be used in place of a in a function,
GHC performs type inference so you don’t have to worry about getting
it right.

Parametric polymorphism
=======================
When you write a polymorphic function, it must work for every possible
input type. The caller gets to pick the type.

All Haskell functions must be parametric in their type parameters;
the functions must not care or make decisions based on the choices
for these parameters. A function can’t do one thing when a is Int
and a different thing when a is Bool.
Haskell simply provides no facility for writing such an operation.
This property of a langauge is called **parametricity**.

There are many deep and profound consequences of parametricity. One
consequence is something called type erasure. Because a running
Haskell program can never make decisions based on type information,
all the type information can be dropped during compilation.
Despite how important types are when writing Haskell code, they are
completely irrelevant when running Haskell code. This property gives
Haskell a huge speed boost when compared to other languages, such as
Python, that need to keep types around at runtime.
(Type erasure is not the only thing that makes Haskell faster, but
Haskell is sometimes clocked at 20x faster than Python.)

In general, given the type of a function, it is possible to figure out
various properties of the function just by thinking about parametricity.
The function must have some way of producing the output type… but where
could values of that type come from? By answering this question, you
can learn a lot about a function.


Total and Partial Functions
===========================
Consider this polymorphic type:
[a] -> a

What functions could have such a type?
The type says that given a list of things of type a, the function must
produce some value of type a. For example, the Prelude function head
has this type.

…But what happens if head is given an empty list as input?
Let’s look at the source code for head…

It crashes! There’s nothing else it possibly could do, since it must
work for all types. There’s no way to make up an element of an arbitrary
type out of thin air.

head is what is known as a partial function: there are certain inputs
for which head will crash. Functions which have certain inputs that will
make them recurse infinitely are also called partial. Functions which
are well-defined on all possible inputs are known as **total** functions.

It is good Haskell practice to avoid partial functions as much as possible.
Actually, avoiding partial functions is good practice in any programming
language—but in most of them it’s ridiculously annoying. Haskell tends
to make it quite easy and sensible.

head is a mistake! It should not be in the Prelude. Other partial Prelude
functions you should almost never use include tail, init, last, and (!!).

Replacing partial functions
===========================
Often partial functions like head, tail, and so on can be replaced by
pattern-matching.

What if you find yourself writing a partial functions?
There are two approaches to take:
(1) change the output type of the function to indicate the possible
    failure. (Maybe)
(2) OK, but what if we know that we will only use head in situations
    where we are guaranteed to have a non-empty list?
    The answer is that if some condition is really guaranteed, then the
    types ought to reflect the guarantee! Then the compiler can enforce
    your guarantees for you. For example:
-}
data NonEmptyList a = NEL a [a]

nelToList :: NonEmptyList a -> [a]
nelToList (NEL x xs) = x:xs

listToNel :: [a] -> Maybe (NonEmptyList a)
listToNel []     = Nothing
listToNel (x:xs) = Just $ NEL x xs

headNEL :: NonEmptyList a -> a
headNEL (NEL a _) = a

tailNEL :: NonEmptyList a -> [a]
tailNEL (NEL _ as) = as


{-
Of course, some properties are more complex and are harder to encode in types.
For example, it might be a critical property in your application that two lists
are permutations of one another. What’s amazing is that Haskell’s type system
is strong enough to encode such properties. Doing so is not for the faint of
heart, and is beyond the scope of this course, but it’s tantalizing to know that
such constructions are possible. If you want to learn more about this, come to
Richard’s office hours, and he’ll be happy to tell you all about so-called
dependent types, which can, in general, enforce arbitrary properties in types.
-}