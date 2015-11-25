-- http://learnyouahaskell.com/functionally-solving-problems#reverse-polish-notation-calculator
--

{-
Protip: it really helps to first think what the type declaration of a function should be
before concerning ourselves with the implementation and then write it down. In Haskell,
a function's type declaration tells us a whole lot about the function, due to the very
strong type system.

-}

import  Data.List as L

getResult :: Either String Double -> Double
getResult (Left err) = error err
getResult (Right x) = x

-- takes as its parameter a string that contains a RPN expression,
-- like "10 4 3 + 2 * -" and gives us back its result.
solveRPN :: String -> Double
solveRPN xs =   let tokens = words xs
                    calcStack = foldl processToken (Right []) tokens
                  in getResult $ validateResult calcStack

processToken :: Either String [Double] -> String -> Either String [Double]
processToken (Left e) _ = Left e
processToken (Right ts) t
        | isBinaryOperator t = processBinaryOperator t ts
        | isUnaryOperator t = processUnaryOperator t ts
        | isAggregateFunction t = processAggregateFunction t ts
        | otherwise = pushStr t ts

validateResult :: Either String [Double] -> Either String Double
validateResult (Left e) = Left e
validateResult (Right [x]) = Right x
validateResult (Right xs) = Left errMsg
                        where errMsg = "Stack when getResult was should be a single element only, but it has " ++ (show (length xs))

pushStr :: String -> [Double] -> Either String [Double]
pushStr x xs =   case (reads x :: [(Double, String)]) of
  []        -> Left ("Not a Number " ++ x)
  [(n, "")] -> Right (push n xs)
  otherwise -> Left ("Bad Number " ++ x)

push :: Double -> [Double] -> [Double]
push x xs = xs ++ [x]

pushResult :: Either String Double -> [Double] -> Either String [Double]
pushResult (Left err) _ = Left err
pushResult (Right x) xs = Right (push x xs)

pop :: [Double] -> (Maybe Double, [Double])
pop [] = (Nothing, [])
pop xs = (l, r)
            where
                l = Just (last xs)
                r = init xs

isBinaryOperator :: String -> Bool
isBinaryOperator x = L.isInfixOf x ":+:-:*:/:^:"

isUnaryOperator :: String -> Bool
isUnaryOperator x = L.isInfixOf x ":ln:"

isAggregateFunction :: String -> Bool
isAggregateFunction x = L.isInfixOf x ":sum:"

processAggregateFunction :: String -> [Double] -> Either String [Double]
processAggregateFunction "sum" xs = pushResult (Right (sum xs)) []

processUnaryOperator :: String -> [Double] -> Either String [Double]
processUnaryOperator op xs =  let
  (arg, rem) = pop xs
  result = processUnaryOperatorWithParams op arg
  in pushResult result rem

processBinaryOperator :: String -> [Double] -> Either String [Double]
processBinaryOperator op xs =  let
  (arg2, rem1) = pop xs
  (arg1, rem2) = pop rem1
  result = processBinaryOperatorWithParams op arg1 arg2
  in pushResult result rem2

processUnaryOperatorWithParams :: String -> Maybe Double -> Either String Double
processUnaryOperatorWithParams op Nothing = Left $ inSuffArgErr op
processUnaryOperatorWithParams "ln" (Just x) = Right (log x)

processBinaryOperatorWithParams :: String -> Maybe Double -> Maybe Double -> Either String Double
processBinaryOperatorWithParams op _ Nothing = Left $ inSuffArgErr op
processBinaryOperatorWithParams op Nothing _ = Left $ inSuffArgErr op
processBinaryOperatorWithParams "+" (Just x) (Just y) = Right (x + y)
processBinaryOperatorWithParams "-" (Just x) (Just y) = Right (x - y)
processBinaryOperatorWithParams "*" (Just x) (Just y) = Right (x * y)
processBinaryOperatorWithParams "/" (Just x) (Just y) = Right (x / y)
processBinaryOperatorWithParams "^" (Just x) (Just y) = Right (x ** y)

inSuffArgErr :: String -> String
inSuffArgErr op = "Insufficient Arguments for Operator " ++ op


tests = let test1 = -4      == solveRPN "10 4 3 + 2 * -"
            test2 =  5      == solveRPN "2 3 +"
            test3 = -3947   == solveRPN "34 12 33 55 66 + * - +"
            test4 = 4037    == solveRPN "90 34 12 33 55 66 + * - + -"
            test5 = 87      == solveRPN "90 3 -"
            test6 = 10      == solveRPN "10 10 10 10 sum 4 /"
            test7 = 100     == solveRPN "10 2 ^"
            test8 = 0.9932517730102834 == solveRPN "2.7 ln"
            allTests = test1 && test2 && test3 && test4 && test5
                        && test6 && test7 && test8
            in allTests
