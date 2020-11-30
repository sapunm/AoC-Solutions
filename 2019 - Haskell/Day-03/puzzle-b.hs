import Data.Maybe (fromJust)

parse :: String -> [[(Char, Int)]]
parse input = map (\line -> map (\(s:sx) -> (s, read sx)) (words [if c == ',' then ' ' else c | c <- line])) (lines input)

segments :: (Int, Int, Int) -> [(Char, Int)] -> [(Char, Int, Int, Int, Int, Bool)]
segments (x, y, steps) ((segType, len):rest)
    | segType == 'L' = ('H', y, x - len, x, steps, True) : segments (x - len, y, steps + len) rest
    | segType == 'R' = ('H', y, x, x + len, steps, False) : segments (x + len, y, steps + len) rest
    | segType == 'D' = ('V', x, y - len, y, steps, True) : segments (x, y - len, steps + len) rest
    | segType == 'U' = ('V', x, y, y + len, steps, False) : segments (x, y + len, steps + len) rest
    | otherwise = []
segments _ [] = []

intersection :: (Char, Int, Int, Int, Int, Bool) -> (Char, Int, Int, Int, Int, Bool) -> Maybe Int
intersection (type1, c1, f1, t1, steps1, reversed1) (type2, c2, f2, t2, steps2, reversed2)
    | type1 == type2 = Nothing
    | c1 > f2 && c1 < t2 && c2 > f1 && c2 < t1 = Just $ steps1 + steps2 + case (reversed1, reversed2) of
                                                                            (False, False) -> c1 - f2 + c2 - f1
                                                                            (False, True) -> t2 - c1 + c2 - f1
                                                                            (True, False) -> c1 - f2 + t1 - c2
                                                                            (True, True) -> t2 - c1 + t1 - c2
    | otherwise = Nothing

intersections :: [(Char, Int, Int, Int, Int, Bool)] -> [(Char, Int, Int, Int, Int, Bool)] -> [Int]
intersections segs1 segs2 = [fromJust i | s1 <- segs1, s2 <- segs2, let i = intersection s1 s2, i /= Nothing]

minIntersection :: [[(Char, Int)]] -> Maybe Int
minIntersection wires
    | null ints = Nothing
    | otherwise = Just $ foldr1 min ints
    where ints = intersections (segments (0, 0, 0) (head wires)) (segments (0, 0, 0) (last wires))

main :: IO ()
main = do
    input <- getContents
    print $ minIntersection (parse input)
