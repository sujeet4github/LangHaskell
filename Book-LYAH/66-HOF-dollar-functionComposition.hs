-- http://learnyouahaskell.com/higher-order-functions#function-application
-- http://learnyouahaskell.com/higher-order-functions#composition
--

-- apart from getting rid of parentheses, $ means that function application can be treated just like another function
-- use $ to map function application over a list of functions
useDollarOpToMapFunctionAppOverListOfFunctions = map ($ 3) [(4+), (10*), (^2), sqrt]


-- Function Composition is much more clear and consise than lambdas
-- e.g. turn list of nos all into negative numbers
makeNegative_usingLambdas = map (\x -> negate $ abs x)
makeNegative_usingComp = map (negate . abs)

-- Function composition is right-associative, so we can compose many functions at a time.
-- The expression f (g (z x)) is equivalent to (f . g . z) x

functionCompUsingFunctionsWithMoreThan1Arg = replicate 100 . product . map (*3) . zipWith max [1,2,3,4,5] $ [4,5,6,7,8]
canReplace = replicate 100 (product (map (*3) (zipWith max [1,2,3,4,5] [4,5,6,7,8])))

withOutPointFreeStyle x = ceiling (negate (tan (cos (max 50 x))))
pointFreeStyle = ceiling . negate . tan . cos . max 50