import Data.List (nub, sort, groupBy)
import Data.Ratio

parse :: String -> [(Int, Int)] -> Int -> Int -> [(Int, Int)]
parse (c:input) asteroids x y
    | c == '#' = parse input ((x, y):asteroids) (x + 1) y
    | c == '.' = parse input asteroids (x + 1) y
    | otherwise = parse input asteroids 0 (y+1)
parse [] asteroids _ _ = asteroids

countFractionals (x, y) asteroids = length $ nub $ map (\(x', y') -> (x'-x) % (y'-y)) asteroids

countVisible asteroids pos@(x, y)
    | null asteroids = (0, 0, 0)
    | otherwise = ((countFractionals pos $ filter (\(x', y') -> y' /= y && x' > x) asteroids)
        + (countFractionals pos $ filter (\(x', y') -> y' /= y && x' < x) asteroids)
        + (min 1 (length (filter (\(x', y') -> x' == x && y' < y) asteroids)))
        + (min 1 (length (filter (\(x', y') -> x' == x && y' > y) asteroids)))
        + (min 1 (length (filter (\(x', y') -> x' < x && y' == y) asteroids)))
        + (min 1 (length (filter (\(x', y') -> x' > x && y' == y) asteroids))), x, y)

findBestPos asteroids = maximum $ map (countVisible asteroids) asteroids

quadrant (x, y)
    | x >= 0 && y < 0 = 1
    | x > 0 && y >= 0 = 2
    | x <= 0 && y > 0 = 3
    | otherwise = 4

angular :: (Int, Int) -> Rational
angular (x, y) =  approxRational ((if q <= 2 then -1.0 else 1.0) * acos (y' / sqrt (x'*x' + y'*y'))) 1e-6
    where (x', y', q) = (fromIntegral x, fromIntegral y, quadrant (x, y))

dist :: (Int, Int) -> Int
dist (x, y) = abs x + abs y

rotateByOne :: [a] -> [a]
rotateByOne = (drop <> take) 1

makeGroups asteroids station@(x, y)
    | null asteroids = []
    | otherwise =  groupBy (\(a, b, _) (a', b', _) -> a == a' && b == b')
                   $ sort
                   $ map (\pos@(x', y') -> let loc = (x'-x, y'-y) in (quadrant loc, angular loc, (dist loc, pos))) $ filter (/=station) asteroids

makeOrder [] ordered = reverse ordered
makeOrder (g:rest) ordered = case take 1 g of
    [] -> makeOrder rest ordered
    [ast] -> makeOrder (rotateByOne ((drop 1 g):rest)) (ast:ordered)

laserCut :: [(Int, Int)] -> [(Int, Rational, (Int, (Int, Int)))]
laserCut asteroids 
    | null asteroids = []
    | otherwise = makeOrder (makeGroups asteroids (x, y)) []
    where bestPos@(_, x, y) = findBestPos asteroids

main :: IO ()
main = do
    input <- getContents
    print $ findBestPos $ parse input [] 0 0
    print $ (laserCut $ parse input [] 0 0) !! 199
