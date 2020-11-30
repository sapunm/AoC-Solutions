import Data.List (nub)
import Data.Ratio

parse :: String -> ([(Int, Int)]) -> Int -> Int -> ([(Int, Int)])
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

main :: IO ()
main = do
    input <- getContents
    print $ findBestPos $ parse input [] 0 0
