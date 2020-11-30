import Data.List (partition)

parse :: String -> [(String, String)]
parse input = map (\x -> (x !! 0, x !! 1)) $ map (\line -> words [if c == ')' then ' ' else c | c <- line]) (lines input)

countOrbits :: Int -> [(String, String)] -> String -> Int
countOrbits depth orbits center
    | null orbits = 0
    | otherwise = depth * (length children) + sum (map (countOrbits (depth + 1) rest) $ map snd children)
        where (children, rest) = partition ((==center) . fst) orbits

main :: IO ()
main = do
    input <- getContents
    print $ countOrbits 1 (parse input) "COM"
