# Salient Features

function names must be lower case, type names are Capitalized, identifiers for values are lower case
- this is enforced by the Haskell lexical syntax

Operators
- Infix - entirely of symbols
- A regular function can be converted to an operator by surrounding with ``
- Fixity Declaration
	- given for any infix operator Or constructor
	- e.g. infixr 9 .  -- assigns a precedence level of 9 to the function composition operator
	- Precedence level is between 0 to 9, with 9 being strongest
		- right associativity is specified by infixr
		- left associativity is specified by infixl
		- non associativity is specified by infix
	- If no fixity declaration is given for a particular operator, it defaults to infixl 9.

Section
- In Haskell the partial application of an infix operator is called a section:
```
	 (x+) is the same as \ y -> x+y
	 (+y) is the same as \ x -> x+y
	 (+) is the same as \ x,y -> x+y
	This is coercing an operator into a functional value
 		Parentheses are mandatory here
 	Function Composition Operator - .
 		f . g x is the same as f ( g(x) )
```

# Layout
use semicolons, braces etc or use a two-dimensional syntax called "layout", essentially relies on declarations being lined up in columns.
  1. the next character following any of the keywords:
      where, let, or of.
    - is what determines the starting column for the declarations in the where, let, or case expression being written
    - Thus we can begin the declarations on the same line as the keyword, the next line, etc.
		- NOTE: the do keyword also uses layout
	2. just be sure that the starting column is further to the right than the starting column associated with the immediately surrounding clause (otherwise it would be ambiguous).
	  - The "termination" of a declaration happens when something appears at or to the left of the starting column
		associated with that binding form.
