parse :: String -> [Int]
parse input = map (read . replicate 1) input

pattern :: Int -> [Int]
pattern iter = drop 1 $ concat $ map (replicate (iter + 1)) $ concat $ repeat [0, 1, 0, -1]

fft input 0 = input
fft input iterations = fft (map oneIteration [0..(length input - 1)]) (iterations - 1)
   where oneIteration iter = (abs $ sum $ zipWith (*) (pattern iter) (input)) `mod` 10

main = do
   input <- fmap parse getContents
   print $ foldl1 (++) $ map (show) $ take 8 $ fft input 100