# Salient Features

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
