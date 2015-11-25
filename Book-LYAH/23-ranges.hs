-- http://learnyouahaskell.com/starting-out#texas-ranges
--

rangeEx1 = [1..20]
rangeEx2 = ['K'..'Z']

rangeExWithStep = [3,6..20]

-- While pretty smart, ranges with steps aren't as smart as some people expect them to be.
-- you can't just do [20..1]
rangeGoingDownHasToBeDoneWithStep = [20,19..1]

-- Watch out when using floating point numbers in ranges! Because they are not completely precise (by definition)
dontUseFloatingPointNumbersInRanges = [0.1, 0.3 .. 1]
