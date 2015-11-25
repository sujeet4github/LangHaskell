-- 93-command-line-args.hs
import System.Environment
import System.Directory
import System.IO
import Data.List

dispatch :: [(String, [String] -> IO () )]
dispatch = [("add", add)
            , ("view", view)
            , ("remove", remove)
            ]

add :: [String] -> IO ()
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n")

view :: [String] -> IO ()
view [fileName] = do
                    contents <- readFile fileName
                    let todoTasks = lines contents
                        numberedTasks = zipWith (\n line -> show n ++ " - " ++ line) [0..] todoTasks
                    putStr $ unlines numberedTasks

remove :: [String] -> IO ()
remove [fileName, numberString] = do
                                    h <- openFile "todo.txt" ReadMode

                                    (tempName, tempHandle) <- openTempFile "." "temp"

                                    contents <- hGetContents h
                                    let todoTasks = lines contents
                                        number = read numberString
                                        newTodoItems = delete (todoTasks !! number) todoTasks

                                    hPutStr tempHandle $ unlines newTodoItems
                                    hClose tempHandle
                                    hClose h

                                    copyFile "todo.txt" "todo.txt.bak"
                                    removeFile "todo.txt"
                                    renameFile tempName "todo.txt"

main = do
        (command: args) <- getArgs
        let (Just action) = lookup command dispatch
        action args