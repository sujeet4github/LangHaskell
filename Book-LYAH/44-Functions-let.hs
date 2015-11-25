-- http://learnyouahaskell.com/syntax-in-functions#let-it-be
--

{-
Very similar to where bindings are let bindings.

Where bindings are a syntactic construct that let you bind to variables
at the end of a function and the whole function can see them, including all the guards.

Let bindings let you bind to variables anywhere and are expressions themselves,
but are very local, so they don't span across guards.

-}

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    -- Notice that the names are also aligned in a single column.
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^2
    in  sideArea + 2 * topArea


-- Difference between Where and Let
-- Difference between Let and Where
-- The difference is that let bindings are expressions themselves - can be used wherever
-- expressions can.
-- Whereas, where bindings are just syntactic constructs.
--
if_then_else_Expr = [if 5 > 3 then "Woo" else "Boo", if 'a' > 'b' then "Foo" else "Bar"]
let_expr_1 = 4 * (let a = 9 in a + 1) + 2
let_expr_introduceFunctionsInALocalScope = [let square x = x * x in (square 5, square 3, square 2)]
let_expr_several_exprs = (let a = 100; b = 200; c = 300 in a*b*c, let foo="Hey "; bar = "there!" in foo ++ bar)

-- calc bmi using list comprehension and let
-- bmi = w / h ^ 2
--
calcBmi :: (RealFloat a) => [(a,a)] -> [a]
calcBmi xs = [ bmi | (w,h) <- xs , let bmi = w / h ^ 2]
-- We omitted the in part of the let binding when we used them in list comprehensions
-- because the visibility of the names is already predefined there.
-- The in part can also be omitted when defining functions and constants directly in GHCi.
-- If we do that, then the names will be visible throughout the entire interactive session.

-- However, we could use a let in binding in a predicate and the names defined would only
-- be visible to that predicate.