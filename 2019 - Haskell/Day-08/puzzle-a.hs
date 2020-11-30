parse :: String -> [Int]
parse input = [read [c] | c <- input, c /= '\n', c /= '\r']

count :: Int -> [Int] -> Int
count n (x:rest) = fromEnum (n == x) + count n rest
count _ []     = 0

getLayers :: Int -> [Int] -> [[Int]]
getLayers size raw
    | null raw = []
    | otherwise = [take size raw] ++ (getLayers size (drop size raw))

getAnswer :: [[Int]] -> Int
getAnswer layers = foldr1 (*) min
    where _:min = minimum [[count e l | e <- [0, 1, 2]] | l <- layers]

main :: IO ()
main = do
    input <- getContents
    print $ getAnswer $ getLayers (25*6) (parse input)