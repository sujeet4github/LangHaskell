-- http://learnyouahaskell.com/starting-out#an-intro-to-lists
--

whatAreLists :: [[Char]]
whatAreLists = ["lists are a homogenous data structure.",
        "It stores several elements of the same type."]

listsDefinedWithSquareBrackets :: [Integer]
listsDefinedWithSquareBrackets = let lostNumbers = [4,8,15,16,23,42] in lostNumbers

puttingListTogetherIsDoneWithPlusPlus :: [Integer]
puttingListTogetherIsDoneWithPlusPlus = let b_def = [9,10,11,12]
                                            in [1,2,3,4] ++ b_def


-- putting something at the beginning of a list using the : operator (also called the cons operator)
-- is instantaneous.
listEfficientAtPuttingAtHead_ConsOperator_1 :: [Char]
listEfficientAtPuttingAtHead_ConsOperator_1 = 'A' : " Small Cat"

listEfficientAtPuttingAtHead_ConsOperator_2 :: [Integer]
listEfficientAtPuttingAtHead_ConsOperator_2 = 1 : [2,3,4,5]

usingIndexOperator :: [Integer]
usingIndexOperator = let b = [[1,2,3,4],[5,3,3,3],[1,2,2,3,4],[1,2,3]]
                        in (b !! 2)

l = [1, 2, 3]
-- [] notation is syntactic sugar for
l1 = 1:(2:(3:[]))
-- operator : is right associative
l2 = 1:2:3:[]
-- Strings notations "" is syntactic sugar
stringNotationIsSyntacticSugar = "abc" == 'a':'b':'c':[]

{-
When using <, <=, > and >= to compare lists, they are compared in lexicographical order.
First the heads are compared.
If they are equal then the second elements are compared, etc.
-}
comparingLists :: (Bool, Bool, Bool, Bool, Bool)
comparingLists = (
    [3,2,1] > [2,1,0],
    [3,2,1] > [2,10,100],
    [3,4,2] > [3,4],
    [3,4,2] > [2,4],
    [3,4,2] == [3,4,2])


listChecks_length = length []
listChecks_null = null []
listChecks_elem = 4 `elem` [3,4,5,6]

listOperations_head = head [5,4,3,2,1]
listOperations_tail = tail [5,4,3,2,1]
listOperations_last = last [5,4,3,2,1]
listOperations_init = init [5,4,3,2,1]
listOperations_reverse = reverse [5,4,3,2,1]
listOperations_take = take 2 [5,4,3,2,1]
listOperations_drop = drop 2 [5,4,3,2,1]
listOperations_minimum = minimum [8,4,2,1,5,6]
listOperations_maximum = maximum [8,4,2,1,5,6]
listOperations_sum = sum [5,2,1,6,3,2,5,7]
listOperations_product = product [6,2,1,2]