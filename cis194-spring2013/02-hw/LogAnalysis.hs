{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module LogAnalysis where


import Log

import Test.QuickCheck
import Test.QuickCheck.All()
-- Monadic Testing Example
import Test.QuickCheck.Monadic as QCM

readNumeric :: String -> Maybe Int
readNumeric str =
-- parses an individual line from the log file
parseMessage :: String -> LogMessage
parseMessage str = logMsg
        where
          wordsInLine = words str
          (messageTypeOpt, restOfWords) = readMessageType wordsInLine
          tsStr = if (null restOfWords) "" else restOfWords !! 0
          timestampOpt = case (reads tsStr :: [(Int, String)]) of
                        [(ts, "")]  -> Just ts
                        _           -> Nothing
          message = unwords (drop 1 restOfWords)
          logMsg = case  messageTypeOpt of
                    Just messageType  ->
                      case timestampOpt of
                        Just timestamp  -> LogMessage messageType timestamp message
                        Nothing         -> Unknown str
                    Nothing           -> Unknown str

-- parse out MessageType and payload from a list of words if possible
readMessageType :: [String] -> (Maybe MessageType, [String])
readMessageType ("W": rest) = (Just Warning, rest)
readMessageType ("I": rest) = (Just Info, rest)
readMessageType ("E": rest) = let errNoStr = rest !! 0
                                in case (reads errNoStr :: [(Int, String)]) of
                                    [(errNo, "")] -> (Just (Error errNo), drop 1 rest )
                                    _             -> (Nothing, rest)
readMessageType wordsInLine = (Nothing, wordsInLine)


-- will be unknown unless specially formatted
prop_parseMessage :: String -> Bool
prop_parseMessage str = Unknown str == parseMessage str
prop_singleTest_parseMessage_ex1 :: Bool
prop_singleTest_parseMessage_ex1 = parseMessage "E 2 562 help help" == LogMessage (Error 2) 562 "help help"
prop_singleTest_parseMessage_ex2 :: Bool
prop_singleTest_parseMessage_ex2 = parseMessage "I 29 la la la" == LogMessage Info 29 "la la la"
prop_singleTest_parseMessage_ex3 :: Bool
prop_singleTest_parseMessage_ex3 = parseMessage "This is not in the right format" ==  Unknown "This is not in the right format"


-- parse a string with multiple lines, each line being a log file line
parse :: String -> [LogMessage]
parse fileContent = map parseMessage (lines fileContent)

-- inserts a new LogMessage into an existing MessageTree, producing a
--  new MessageTree
-- insert may assume that it is given a sorted MessageTree, and must produce a
-- new sorted MessageTree containing the new LogMessage in addition to the
-- contents of the original MessageTree.
insert :: LogMessage -> MessageTree -> MessageTree
insert (Unknown _) messageTree = messageTree
insert logMessage Leaf = Node Leaf logMessage Leaf
insert logMessage@(LogMessage _ messageTimestamp _) (Node left (LogMessage messageType nodeTimestamp payload) right)
  | messageTimestamp > nodeTimestamp = (Node left (LogMessage messageType nodeTimestamp payload) (insert logMessage right))
  | otherwise = (Node (insert logMessage left) (LogMessage messageType nodeTimestamp payload) right)
insert (LogMessage _ _ _) (Node _ (Unknown _) _) = Leaf

prop_insert_onEmptyTreeWorks :: LogMessage -> Bool
prop_insert_onEmptyTreeWorks mesg@(Unknown _) = Leaf == (insert mesg Leaf)
prop_insert_onEmptyTreeWorks logMessage = (Node Leaf logMessage Leaf) == (insert logMessage Leaf)


-- build a complete MessageTree from a list of messages
build :: [LogMessage] -> MessageTree
build logMessages = foldl (\acc x -> insert x acc) Leaf logMessages

-- does an in-order traversal of the MessageTree
-- takes a sorted MessageTree and produces a list of all the
-- LogMessages it contains, sorted by timestamp from smallest to biggest.
inOrder :: MessageTree -> [LogMessage]
inOrder Leaf = []
inOrder (Node left logMessage right) = leftTree ++ [logMessage] ++ rightTree
                                    where
                                        leftTree = (inOrder left)
                                        rightTree = (inOrder right)


-- extract the relevant information from log messages
-- We have decided that “relevant” means “errors with a severity of at least 50”.
whatWentWrong :: [LogMessage] -> [String]
whatWentWrong messages = filter (/= "") (map extractRelevantErrorMessage (inOrder (build messages)))

extractRelevantErrorMessage :: LogMessage -> String
extractRelevantErrorMessage (LogMessage (Error severity) _ payload)
  | severity >= 50  = payload
  | otherwise       = ""
extractRelevantErrorMessage _ = ""


prop_extractRelevantErrorMessage :: LogMessage -> Bool
prop_extractRelevantErrorMessage message = expectedResult == (extractRelevantErrorMessage message)
  where expectedResult = case message of
                          LogMessage (Error en) _ payload -> if (en >= 50) then payload else ""
                          _                                 -> ""
instance Arbitrary LogMessage where
    arbitrary = do
      n <- choose (0,3) :: Gen Int
      case n of
        0 -> do
                msg <- arbitrary
                return (Unknown msg)
        1 -> do ts <- arbitrary
                msg <- arbitrary
                return (LogMessage Info ts msg)
        2 -> do ts <- arbitrary
                msg <- arbitrary
                return (LogMessage Warning ts msg)
        3 -> do ts <- arbitrary
                eno <- arbitrary
                msg <- arbitrary
                return (LogMessage (Error eno) ts msg)
        _ -> return (Unknown "")
instance Arbitrary MessageTree where
  arbitrary = do
    n <- choose (1,5) :: Gen Int
    message <- arbitrary :: Gen LogMessage
    leftTree <- arbitrary :: Gen MessageTree
    rightTree <- arbitrary :: Gen MessageTree
    case n of
      1 -> do
            return Leaf
      2 -> do
            return (Node Leaf message Leaf)
      3 -> do
            return (Node leftTree message Leaf)
      4 -> do
            return (Node Leaf message rightTree)
      5 -> do
            return (Node leftTree message rightTree)
      _ -> return Leaf

prop_singleTest_withSampleLog :: Property
prop_singleTest_withSampleLog = QCM.monadicIO $ do
    str <- QCM.run (testWhatWentWrong parse whatWentWrong "sample.log")
    QCM.assert $ ["Way too many pickles","Bad pickle-flange interaction detected","Flange failed!"] == str


-- Wacky boilerplate to make all tests run.
return []
-- run this to test all quickcheck properties
runTests :: IO Bool
runTests = $quickCheckAll
