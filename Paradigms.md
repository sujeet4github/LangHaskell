# Paradigms

"I learnt that Functor and Applicative give you pragmatic ways to handle a million different
complex data structures and abstract data types without caring about their implementation.
I learnt that Monad gives you rational ways to structure logic and the order of computations,
giving you more power than in an imperative language I know ("programmable semicolons!").
I learnt that you can handle errors in pure and explicit ways.
I discovered that almost everything can be composable;
I can have a thousand computations that may fail, run them in parallel trivially and still
catch all those errors in a single line while using the same operators I use to print text
to the screen. I fell in love with currying and how easily things can work together if the
language lets them. Also I learnt that concurrency can be a beautifully simple endeavour,
that there are actually a lot of ways to do it and that it actually makes things faster
without adding unnecessary complexity. I learnt how rich types can give structure, meaning
and modularity to a piece of code (almost) for free.
And all this in one language, with a great package manager, a lovely collection of well thought
libraries, an industrial strength compiler, testing tools, profiling options, and a great
community full of the smartest people I've ever met."

### Purely Functional
Haskell has no notion of "assignment", "mutable state", "variables", and is a "pure functional language",
which means that every function called with the same input parameters will return exactly the same result.

### Types
- Haskell is a typeful language - statically typed along with type inferencing
- type constructors with type variables (compile-time) vs data/value constructors
- Hindley-Milner type system, which forms the basis of the type systems of Haskell, ML, Miranda,4 and several other
(mostly functional) languages.
- polymorphic types (with type variables) <==> generics in java
- It is important to distinguish between applying a data constructor to yield a value, and applying a type constructor to yield a type; the former happens at run-time and is how we compute things in Haskell, whereas the latter happens at compile-time and is part of the type system's process of ensuring type safety.
