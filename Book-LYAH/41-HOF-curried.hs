-- http://learnyouahaskell.com/higher-order-functions#curried-functions
--


-- By calling functions with too few parameters, so to speak, we're creating new functions on the fly.
-- These are partially applied functions, meaning a function that takes as many parameters as we left out.

multThree :: (Num a) => a -> a -> a -> a
multThree x y z = x * y * z


test_MultThree = lhs == rhs
                 where
                    lhs             = multThree 3 5 9
                    multTwoWith3            = multThree 3
                    multOneWith3AndThen5    = multTwoWith3 5
                    rhs                     = multOneWith3AndThen5 9