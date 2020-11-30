parse :: String -> [Int]
parse input = map (read . replicate 1) input

getOffset :: [Int] -> Int
getOffset input = sum $ zipWith (*) (take 7 input) (map (10^) [6, 5..0])

getSignal :: [Int] -> Int -> Int -> [Int]
getSignal input _ 0 = input
getSignal input numElements iteration = getSignal currentPhase numElements (iteration - 1)
   where currentPhase = reverse $! take numElements $ map (`mod` 10) $ scanl1 (+) $ concat $ repeat $ reverse input

main = do
   input <- fmap parse getContents
   print $ foldl1 (++) $ map (show) $ take 8 $ getSignal input (10000 * (length input) - (getOffset input)) 100
