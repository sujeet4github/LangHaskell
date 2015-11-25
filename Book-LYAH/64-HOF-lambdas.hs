-- http://learnyouahaskell.com/higher-order-functions#lambdas
--

{-
Lambdas are expressions, that's why we can just pass them like that.

The expression (\xs -> length xs > 15) returns a function that tells us whether
the length of the list passed to it is greater than 15.

People who are not well acquainted with how currying and partial application
works often use lambdas where they don't need to.

-}

lambdasCanTakeAnyNumberOfParams =   let l1 = [5,4,3,2,1]
                                        l2 = [1,2,3,4,5]
                                    in
                                        zipWith (\x y -> (x*30 + 3) / y) l1 l2

-- If a pattern matching fails in a lambda, a runtime error occurs, so be careful when pattern matching in lambdas!
patternMatchWorks = let l = [(1,2),(3,5),(6,3),(2,6),(2,5)]
                    in
                        map (\(a,b) -> a+b) l
