
{-- Quick Sort using a for comprehension
	 <- are generators
	 boolean expressions called guards
-}

-- quicksort ::		[a] -> [a]
quicksort [] =		[]
quicksort (x:xs) = 	quicksort [y | y <- xs, y < x]
					++ x
					++ quicksort [y | y <-xs, y >= x]
