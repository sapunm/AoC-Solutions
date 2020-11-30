parse :: String -> [Int]
parse input = [read [c] | c <- input, c /= '\n', c /= '\r']

count :: Int -> [Int] -> Int
count n (x:rest) = fromEnum (n == x) + count n rest
count _ []     = 0

getLayers :: Int -> [a] -> [[a]]
getLayers size raw
    | null raw = []
    | otherwise = [take size raw] ++ (getLayers size (drop size raw))

mergeLayers :: [[Int]] -> [Int]
mergeLayers layers = case layers of
    [layer] -> layer
    layer1:layer2:rest -> mergeLayers $ [if l2 == 2 then l1 else l2 | (l1, l2) <- zip layer1 layer2] : (drop 2 layers)

showLayer :: [Int] -> String
showLayer layer = [if l == 1 then '#' else ' ' | l <- layer]

main :: IO [()]
main = do
    input <- getContents
    sequence $ map print $ getLayers 25 $ showLayer $ mergeLayers $ reverse (getLayers (25*6) (parse input))