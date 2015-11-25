-- http://learnyouahaskell.com/input-and-output#files-and-streams
--

{-

getContents :: IO String
    is an I/O action that reads everything from the standard input until it
    encounters an end-of-file character.

    it does lazy I/O

    getContents is really useful when we're piping the output from one program
    into the input of our program.

interact :: (String -> String) -> IO ()
    The pattern of getting some string from the input, transforming it with a
    function and then outputting that is so common that there exists a function
    which makes that even easier, called interact.

    interact takes a function of type String -> String as a parameter and returns
    an I/O action that will take some input, run that function on it and then
    print out the function's result.

-}

import System.IO


respondPalindromes = unlines . map (\xs -> if isPalindrome xs then "palindrome" else "not a palindrome") . lines
    where
        isPalindrome xs = xs == reverse xs
main1 = interact respondPalindromes

{-
import System.IO

has

System.IO.openFile :: FilePath -> GHC.IO.IOMode.IOMode -> IO GHC.IO.Handle.Types.Handle

    takes a FilePath (type synonym for string)
        type FilePath = String
    and an IOMode
        data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
    and returns an I/O action that will open a file and
    have the file's associated handle encapsulated as its result.

System.IO.hGetContents :: GHC.IO.Handle.Types.Handle -> IO String

    getContents will automatically read from the standard input (that is from the terminal),
    whereas hGetContents takes a file handle which tells it which file to read from.
    In all other respects, they work the same.

hGetLine, hPutStr, hPutStrLn, hGetChar
    They work just like their counterparts without the h, only they take a handle
    as a parameter and operate on that specific file instead of operating on standard
    input or standard output.

System.IO.hClose :: GHC.IO.Handle.Types.Handle -> IO ()

    takes a handle and returns an I/O action that closes the file.

System.IO.withFile
  :: FilePath
     -> GHC.IO.IOMode.IOMode
     -> (GHC.IO.Handle.Types.Handle -> IO r)
     -> IO r

    It takes a path to a file, an IOMode and then it takes a function that takes a handle
    and returns some I/O action. What it returns is an I/O action that will open that file,
    do something we want with the file and then close it.
    The result encapsulated in the final I/O action that's returned is the same as the
    result of the I/O action that the function we give it returns.

System.IO.hSetBuffering
  :: GHC.IO.Handle.Types.Handle
     -> GHC.IO.Handle.Types.BufferMode -> IO ()
    It takes a handle and a BufferMode and returns an I/O action that sets the buffering
    BufferMode is a simple enumeration data type and the possible values it can hold are:
    NoBuffering, LineBuffering or BlockBuffering (Maybe Int).
    The Maybe Int is for how big the chunk should be, in bytes.
    If it's Nothing, then the operating system determines the chunk size.
    NoBuffering means that it will be read one character at a time.

System.IO.hFlush :: GHC.IO.Handle.Types.Handle -> IO ()
    takes a handle and returns an I/O action that will flush the buffer of the file associated with the handle

Loading files and then treating their contents as strings is so common that we have these three
nice little functions to make our work even easier:

-- see deleteTodo.hs

readFile :: FilePath -> IO String
writeFile :: FilePath -> String -> IO ()
appendFile :: FilePath -> String -> IO ()

-}
