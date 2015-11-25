-- http://learnyouahaskell.com/syntax-in-functions#where
--

-- The names we define in the where section of a function are only visible to that function,
-- so we don't have to worry about them polluting the namespace of other functions.
-- Notice that all the names are aligned at a single column.
-- If we don't align them nice and proper, Haskell gets confused because then it doesn't know
-- they're all part of the same block.
bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "You're underweight, you emo, you!"
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"
    | otherwise     = "You're a whale, congratulations!"
    where bmi = weight / height ^ 2
        -- use where bindings to pattern match
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

-- where bindings can also be nested.
-- It's a common idiom to make a function and define some helper function in its where clause
-- and then to give those functions helper functions as well, each with its own where clause.

