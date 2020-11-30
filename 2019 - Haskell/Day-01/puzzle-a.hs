parse :: String -> [Integer]
parse input = map read (lines input)

main :: IO ()
main = do
    input <- getContents
    print $ sum (map (\mass -> mass `div` 3 - 2) (parse input))
