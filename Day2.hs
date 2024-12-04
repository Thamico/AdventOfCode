import System.IO

--part 1
isOrdered :: [Int] -> Bool
isOrdered xs = all (\(a, b) -> abs (a - b) >= 1 && abs (a - b) <= 3) (zip xs (tail xs)) && (isAscending xs || isDescending xs)
  where
    isAscending (x:y:ys) = x <= y && isAscending (y:ys)
    isAscending _ = True
    isDescending (x:y:ys) = x >= y && isDescending (y:ys)
    isDescending _ = True

-- part 2
isSafeWithDampener :: [Int] -> Bool
isSafeWithDampener xs = isOrdered xs || any (isOrdered . removeAt xs) [0..length xs - 1]
  where
    removeAt :: [a] -> Int -> [a]
    removeAt xs n = let (ys, zs) = splitAt n xs in ys ++ tail zs

main :: IO ()
main = do
    contents <- readFile "Day2.input"
    let linesOfNumbers = map (map read . words) (lines contents) :: [[Int]]
    let safeCount = length $ filter isSafeWithDampener linesOfNumbers
    putStrLn $ "Number of safe lines: " ++ show safeCount