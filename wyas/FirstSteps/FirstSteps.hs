-- to compile:
-- > stack ghc FirstSteps.hs
-- to run
-- > stack runghc FirstSteps.hs
module FirstSteps where

import           System.Environment

main
   -- Ex 1
   -- reads two arguments from the command line, and prints out a message using both of them
 = do
  args <- getArgs
  let x = args !! 0
  let y = args !! 1
  putStrLn ("Hello, " ++ x ++ " and " ++ y)
   -- Ex 2
   -- Change the program so it performs a simple arithmetic operation on the two
   -- arguments and prints out the result. You can use read to convert a string
   -- to a number, and show to convert a number back into a string.
   -- Play around with different operations.
  let n1 = read (args !! 2)
  let g = args !! 3
  let n2 = read g
  putStrLn ("Oh, and " ++ (show n1) ++ " + " ++ (show n2) ++ " is " ++ (show (n1 + n2)))
   -- Ex 3
   -- getLine is an IO action that reads a line from the console and returns it as a string.
   -- Change the program so it prompts for a name, reads the name, and then prints that
   -- instead of the command line value
  putStrLn ("Gimme> ")
  line <- getLine
  putStrLn ("You entered " ++ line)
