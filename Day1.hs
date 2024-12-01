import Data.List (sort)

-- this is the first half for the assignement
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

-- this is the second assignment
getSimilarityScore :: (Eq a, Num a) => [a] -> [a] -> a
getSimilarityScore xs ys = go xs ys 0
    where 
    go [] _ acc = acc
    go (x:xs) ys acc
        | x `elem` ys = go xs (filter (/= x) ys) (acc + x * count x ys)
        | otherwise = go xs ys acc
    count x = fromIntegral . length . filter (== x)
     
-- main output for the first assignment
--main :: IO ()
--main = do
  --  (leftArray, rightArray) <- readFileAndSplit "Day1.input"
    --let distance = getDistance leftArray rightArray
    --putStrLn $ "Distance between Left Array and Right Array: " ++ show distance

main :: IO ()
main = do
     (leftArray, rightArray) <- readFileAndSplit "Day1.input"
     let similarityScore = getSimilarityScore leftArray rightArray
     putStrLn $ "Similarity Score between Left Array and Right Array: " ++ show similarityScore