-- http://learnyouahaskell.com/syntax-in-functions#case-expressions
--

-- Pattern Matchin (21-*) vs Case Expressions
-- pattern matching on function parameters can only be done when defining functions,
--  case expressions can be used pretty much anywhere.
-- Pattern Matching in function defns is syntactic sugar for Case Expressions

describeList :: [a] -> String
describeList xs = "The list is " ++ case xs of [] -> "empty."
                                               [x] -> "a singleton list."
                                               xs -> "a longer list."