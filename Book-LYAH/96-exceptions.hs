-- http://learnyouahaskell.com/input-and-output#exceptions
--

{-
main = toTry `catch` handler

toTry :: IO ()
toTry = do (fileName:_) <- getArgs
           contents <- readFile fileName
           putStrLn $ "The file has " ++ show (length (lines contents)) ++ " lines!"

handler :: IOError -> IO ()
handler e
    | isDoesNotExistError e = putStrLn "The file doesn't exist!"
    | otherwise = ioError e


There are several predicates that act on IOError and if a guard doesn't evaluate to True,
evaluation falls through to the next guard. The predicates that act on IOError are:

isAlreadyExistsError
isDoesNotExistError
isAlreadyInUseError
isFullError
isEOFError
isIllegalOperation
isPermissionError
isUserError

System.IO.Error also exports functions that enable us to ask our exceptions for some attributes,
like what the handle of the file that caused the error is, or what the filename is.
These start with ioe.


Now you know how to deal with I/O exceptions!
Throwing exceptions from pure code and dealing with them hasn't been covered here, mainly because,
like we said, Haskell offers much better ways (Maybe, Either) to indicate errors than reverting
to I/O to catch them.

Even when glueing together I/O actions that might fail, I prefer to have their type be something
like IO (Either a b), meaning that they're normal I/O actions but the result that they yield when
performed is of type Either a b, meaning it's either Left a or Right b.

-}