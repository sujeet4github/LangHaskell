# What is Canonical


1. It is good Haskell practice to avoid partial functions as much as possible.
     Prelude partial functions you should almost never use include tail, init, last, and (!!)
     From cis194-03-recursion-polymorhism-prelude-etc.hs
2. Avoid writing partial functions too. see cis194 - 03-recursion-polymorhism-prelude-etc.hs
3. Avoid explicit recursion etc use library implementations that use recursion instead
4. Infinite lists
     - avoid filter use takeWhile instead
     - prefer foldr to foldl, verify if foldl' from Data.List is needed
       - refer to foll link for performance concerns: (http://haskell.org/haskellwiki/Foldr_Foldl_Foldl%27)
5. Avoiding Parentheses
     1. If you align your code, compiler will guess the beginnings and endings of syntactic blocks. (see Layout)
     2. $ operator for function calls - lowest precedence...
          - f(5+6) or f $ 5+6
     3. composition operator .
          f(g(h(x)) or f.g.h x
6. Layout and Style Guide
     - http://www.seas.upenn.edu/~cis552/13fa/styleguide.html
7. But sometimes, Maybe a isn't good enough because Nothing doesn't really convey much information other than that something has failed.
    when we're interested in how some function failed or why, we usually use the result type of Either a b errors use the Left value constructor while results use Right.
8. Pro tip: it really helps to first think what the type declaration of a function should be before concerning ourselves with the implementation and then write it down. In Haskell, a function's
    type declaration tells us a whole lot about the function, due to the very strong type system.
9. dependent types, which can, in general, enforce arbitrary properties in types.
