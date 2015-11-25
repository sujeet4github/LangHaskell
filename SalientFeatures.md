# Salient Features

function names must be lower case, type names are Capitalized, identifiers for values are lower case
- this is enforced by the Haskell lexical syntax

### Operators
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

### A Haskell program is a collection of *Modules*.
Modules in Haskell are used for:
- controlling name-spaces
- creating Abstract Data Types

### Section
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

### Let vs Where

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

# Types
Just as expressions denote values, type-expressions denote type values
```
	5 :: Integer
	'a' :: Char
	inc :: Integer -> Integer
	[1, 2, 3] :: [Integer]
	('a', 4) :: (Char, Integer)
```
# User defined types
type constructors with type variables (compile-time) vs data/value constructors

- type: make synonyms
	- syntax: type <newtype> = <existingtype>
- newtype: take existing types and wrap them in new types, mostly so its easier to make them instances of certain type classes
- data: make a brand new type
	- syntax: data <name-of-type> [type-expressions] = <constructor> <value-expression> [deriving ....]
