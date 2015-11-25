-- http://learnyouahaskell.com/starting-out#babys-first-functions
--

{-
The apostrophe doesn't have any special meaning in Haskell's syntax.
It's a valid character to use in a function name.

We usually use ' to either denote a strict version of a function (one that isn't lazy)
or a slightly modified version of a function or a variable. 
-}

-- functions can't begin with uppercase letters
conanO'Brien = "It's a-me, Conan O'Brien!"

-- When a function doesn't take any parameters, we usually say it's a definition (or a name)