-- 92-files-streams.hs
import System.IO
import System.Directory
import Data.List

main =
    do
        h <- openFile "todo.txt" ReadMode

        (tempName, tempHandle) <- openTempFile "." "temp"

        contents <- hGetContents h
        let todoTasks = lines contents
            numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks

        putStrLn "These are your TO-DO items:"
        putStr $ unlines numberedTasks

        putStrLn "Which one do you want to delete?"
        numberString <- getLine
        let number = read numberString
            newTodoItems = delete (todoTasks !! number) todoTasks

        hPutStr tempHandle $ unlines newTodoItems
        hClose tempHandle
        hClose h

        removeFile "todo.txt"
        renameFile tempName "todo.txt"
