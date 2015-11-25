-- http://learnyouahaskell.com/functionally-solving-problems#heathrow-to-london
--

import System.Directory

pathsFile :: FilePath
pathsFile = "paths.txt"
main = do
            fileExists <- doesFileExist pathsFile
            if fileExists
                then processPaths pathsFile
                else createAndProcessPaths pathsFile

processPaths :: FilePath -> IO ()
processPaths f = do
                    contents <- readFile f
                    let
                        contentsList= lines contents
                        listNums    = map read contentsList :: [Int]
                        threes = groupsOf 3 listNums
                        roadSystem  = map (\[a,b,c] -> Section a b c) threes
                        path = optimalPath roadSystem
                        pathString  = concat $ map (show . fst) path
                        pathPrice   = sum $ map snd path
                    putStrLn $ "Road System :" ++ (show roadSystem)
                    putStrLn $ "Best Path is:" ++ pathString
                    putStrLn $ "Prices is   :" ++ show pathPrice

createAndProcessPaths :: FilePath -> IO ()
createAndProcessPaths f = do
                            let paths = "50\n10\n30\n5\n90\n20\n40\n2\n25\n10\n8\n0"
                            writeFile f paths
                            processPaths f

-- that takes a list and splits it into groups of the same size
groupsOf :: Int -> [a] -> [[a]]
groupsOf n _ | n <= 0 = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

data Section = Section { getA :: Int, getB :: Int, getC :: Int } deriving (Show)
type RoadSystem = [Section]

heathrowToLondonHardCoded :: RoadSystem
heathrowToLondonHardCoded = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]


data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]


optimalPath :: RoadSystem -> Path
optimalPath roadSystem =
    let
        (bestAPath, bestBPath) = foldl roadStep ([],[]) roadSystem
        aPathCost = sum (map snd bestAPath)
        bPathCost = sum (map snd bestBPath)
    in
        if aPathCost <= bPathCost
            then reverse bestAPath
            else reverse bestBPath

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) =
    let
        priceA = sum $ map snd pathA
        priceB = sum $ map snd pathB
        forwardPriceToA = priceA + a
        crossPriceToA   = priceB + b + c
        forwardPriceToB = priceB + b
        crossPriceToB   = priceA + a + c
        newPathToA      = if forwardPriceToA <= crossPriceToA
                            then    (A, a): pathA
                            else    (C, c):(B, b): pathB
        newPathToB      = if forwardPriceToB <= crossPriceToB
                            then    (B, b): pathB
                            else    (C, c):(A, a): pathA
    in
        (newPathToA, newPathToB)
