import Data.Maybe (fromJust)

parse :: String -> [[(Char, Int)]]
parse input = map (\line -> map (\(s:sx) -> (s, read sx)) (words [if c == ',' then ' ' else c | c <- line])) (lines input)

segments :: (Int, Int) -> [(Char, Int)] -> [(Char, Int, Int, Int)]
segments (x, y) ((segType, len):rest)
    | segType == 'L' = ('H', y, x - len, x) : segments (x - len, y) rest
    | segType == 'R' = ('H', y, x, x + len) : segments (x + len, y) rest
    | segType == 'D' = ('V', x, y - len, y) : segments (x, y - len) rest
    | segType == 'U' = ('V', x, y, y + len) : segments (x, y + len) rest
    | otherwise = []
segments _ [] = []

intersection :: (Char, Int, Int, Int) -> (Char, Int, Int, Int) -> Maybe Int
intersection (st1, c1, f1, t1) (st2, c2, f2, t2)
    | st1 == st2 = Nothing
    | c1 > f2 && c1 < t2 && c2 > f1 && c2 < t1 = Just $ abs c1 + abs c2
    | otherwise = Nothing

intersections :: [(Char, Int, Int, Int)] -> [(Char, Int, Int, Int)] -> [Int]
intersections segs1 segs2 = [fromJust i | s1 <- segs1, s2 <- segs2, let i = intersection s1 s2, i /= Nothing]

minIntersection :: [[(Char, Int)]] -> Maybe Int
minIntersection wires
    | null ints = Nothing
    | otherwise = Just $ foldr1 min ints
    where ints = intersections (segments (0, 0) (head wires)) (segments (0, 0) (last wires))

main :: IO ()
main = do
    input <- getContents
    print $ minIntersection (parse input)
