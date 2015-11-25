{-
every i/o operation returns a value.
the return type is tagged with IO type.
e.g.
	getChar :: IO Char
	putChar :: Char -> IO ()

	getLine :: IO String

Actions are sequenced using the Monad bind operator (>>=), or syntactic sugar (do)

The IO Monad constitues a small imperative sub-language within Haskell (see implementation of getLine'),
however there are no special semantics for the user, and equational reasoning is not compromised.

-}

-- main is defined to be the entry point of a Haskell program
-- (similar to the main function in C), and must have an IO type, usually IO (). (The name main
-- is special only in the module Main; we will have more to say about modules later.)
main :: IO ()
main = do
		c <- getChar
		putChar c


getLine'	:: IO String
getLine'	= do
				c <- getChar
				if c == '\n'
					then return ""
				else
					do
						l <- getLine
						return (c:l)
{-
Note the second do in the else clause.
Each do introduces a single chain of statements. Any intervening construct, such as the if,
must use a new do to initiate further sequences of actions.
-}


-- IO actions are ordinary values,
-- consider this list of actions
todoList			:: [IO ()]
todoList			= [putChar 'a',
						do
							putChar 'b',
							putChar 'c',
						do
							c <- getChar
							putChar c]
-- this list does not invoke any actions, it simply holds them, to join them, use something like -
sequence_'			:: [IO ()] -> IO ()
sequence_' []		= return ()
sequence_' (a:as)	= do
							a
							sequence_' as
-- For a more Monadic way...
-- TODO:

-- errors are handled using data type - IOError