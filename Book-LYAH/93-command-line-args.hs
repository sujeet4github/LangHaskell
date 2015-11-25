-- http://learnyouahaskell.com/input-and-output#command-line-arguments
--

{-

The System.Environment module has two cool I/O actions.

One is getArgs, which has a type of getArgs :: IO [String] and is an I/O action
that will get the arguments that the program was run with and have as its contained
result a list with the arguments.

getProgName has a type of getProgName :: IO String and is an I/O action that
contains the program name.

-}

-- see todo.hs
