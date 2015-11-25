-- http://learnyouahaskell.com/starting-out#texas-ranges
--

infiniteListUsingRanges = take 24 [13,26..]

infiniteList_cycle = take 10 $ cycle [1,2,3]

infiniteList_repeat = take 10 $ repeat 42