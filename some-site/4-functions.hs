import Test.QuickCheck

{-
Haskell has only 1 argument functions.
all else are curried
Operators are covered in 1-syntax.hs
-}

-- functions in Haskell are non-strict
-- A function f is said to be strict if, when applied to a nonterminating expression, it also fails to
-- terminate. For most programming languages, all functions are strict. But this is not so in Haskell.

-- Non Strict functions are also called Lazy functions, since const1 does not need the value of its
-- argument, it never attempts to evaluate it, and thus does not get caught in a non-terminating computation

const1 x = 1

-- In the following, bot is a non-terminating expression, Expressions that result in some kind of a run-time
--  error, such as divide by 0, also have this value. Such an error is not recoverable:
--  programs will not continue past these errors.
-- Errors encountered by the I/O system, such as an end-of-file error, are recoverable
--  and are handled in a different manner
--
bot = bot

-- the division by zero does not bother the functionality of the const1 function
prop_verify_const1_nonStrictNess = const1 (1 / 0) == 1

-- Pattern Matching (see also 1-syntax.hs - function len)
-- ================

contrived :: ([a], Char, (Int, Float), String, Bool) -> Bool
contrived ([], 'b', (1, 2.0), "hi", True) = False
-- calling contrived with any other arguments throws an exception

-- Special Pattern matching - as-patterns
f s @ (x:xs) = x:s
prop_verify_as_pattern_use = (f.f.f) [1] == [1,1,1,1]

-- Special pattern matching - guards (top level patterns can have a boolean guard)
-- Special pattern matching - wild cards for value we really care nothing about
head'(x:_)	= x
tail'(_:xs)	= xs
prop_verify_wildCardPatterns xs | xs /= [] = tail(xs) == tail'(xs) && head(xs) == head'(xs)
-- to handle empty lists - there may be a better way to do this in QuickCheck
prop_verify_wildCardPatterns [] = True

{-
Pattern matching can either fail, succeed or diverge.
A successful match binds the formal parameters in the pattern.
Divergence occurs when a value needed by the pattern contains an error (non-terminal expression).

The matching process itself occurs top-down, left-to-right.

Failure of a pattern anywhere in one equation results in failure of the whole equation,
and the next equation is then tried. If all equations fail, the value of the function
application is non-terminal-expression, and results in a run-time error.

-}

-- Function Syntax:
-- function-name [pattern-expression [| guard-expression-boolean] = function-definition

-- Case Expressions
-- ================
-- alternative to creating a new function for every match
fdef 1 2 3 = "1"
fdef 1 3 2 = "2"
fdef 0 0 0 = "0"
-- The following is equivalent to above 3 defns
fdef' x y z = case (x, y, z) of
				(1, 2, 3) -> "1"
				(1, 3, 2) -> "2"
				(0, 0, 0) -> "0"

-- Lazy Patterns
--
