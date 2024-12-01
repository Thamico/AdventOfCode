import Data.List (sort)


readFileAndSplit :: FilePath -> IO ([Int], [Int])
readFileAndSplit filename = do
    contents <- readFile filename
    let linesOfFiles = lines contents
        (leftArray, rightArray) = unzip $ map ((\[x, y] -> (read x, read y)) . words) linesOfFiles
    return (leftArray, rightArray)


getDistance :: (Ord a, Num a) => [a] -> [a] -> a
getDistance xs ys = go (sort xs) (sort ys) 0
    where
    go [] [] acc = acc
    go (x:xs) (y:ys) acc = go xs ys (acc + abs (x - y))
    go _ _ acc = acc

main :: IO ()
main = do
    (leftArray, rightArray) <- readFileAndSplit "Day1.input"
    let distance = getDistance leftArray rightArray
    putStrLn $ "Distance between Left Array and Right Array: " ++ show distance