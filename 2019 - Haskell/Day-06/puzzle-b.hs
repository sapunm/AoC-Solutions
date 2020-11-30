import Data.List (partition)

parse :: String -> [(String, String)]
parse input = map (\x -> (x !! 0, x !! 1)) $ map (\line -> words [if c == ')' then ' ' else c | c <- line]) (lines input)

pathTo :: String -> [String] -> [(String, String)] -> String -> [String]
pathTo object path orbits center
    | object == center = reverse path
    | null orbits = []
    | otherwise = foldr (++) [] (map (pathTo object (center:path) rest) $ map snd children)
        where (children, rest) = partition ((==center) . fst) orbits

countTransfers :: String -> String -> [(String, String)] -> String -> Int
countTransfers fromObject toObject orbits center = length p1 + length p2 - 2 * length (takeWhile (\(x, y) -> x == y) $ zip p1 p2)
    where (p1, p2) = (pathTo fromObject [] orbits center, pathTo toObject [] orbits center)

main :: IO ()
main = do
    input <- getContents
    print $ countTransfers "YOU" "SAN" (parse input) "COM"
