import Data.List (transpose)

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

parse :: String -> [[Int]]
parse input = map (\ line -> map read $ words [c | c <- line, not (c `elem` "xyz=,<>")]) $ lines input

calcPositions :: [[Int]] -> [[Int]] -> [[Int]]
calcPositions positions velocities = map (\(p, v) -> ((\m -> map sum m) . transpose) [p, v]) $ zip positions velocities

calcVelocitiesDelta :: [[Int]] -> [[Int]]
calcVelocitiesDelta positions = map ((\m -> map sum m) . transpose) $ splitEvery 3 [ let (p1,p2) = (snd m1,snd m2) in map (\(x, y) -> signum (y - x)) (zip p1 p2) | m1 <- moons, m2 <- moons, fst m1 /= fst m2]
    where moons = zip [1..] positions

calcVelocities :: [[Int]] -> [[Int]] -> [[Int]]
calcVelocities positions velocities = map (\(p, v) -> ((\m -> map sum m) . transpose) [p, v]) $ zip delta velocities
    where delta = calcVelocitiesDelta positions

step positions velocities = ((calcPositions positions newVelocities), newVelocities)
    where newVelocities = calcVelocities positions velocities

simulation positions velocities = (positions, velocities) : simulation newPositions newVelocities
        where (newPositions, newVelocities) = step positions velocities

energy (positions, velocities) = sum $ map (\(x, y) -> moonEnergy x y) $ zip positions velocities
    where moonEnergy x y = (sum $ map abs x) * (sum $ map abs y)
    

main :: IO ()
main = do
    input <- getContents
    print $ energy $ (simulation (parse input) (replicate 4 [0, 0, 0])) !! 1000
    -- print $ step (parse input) (replicate 4 [0, 0, 0])
    -- print $ calcVelocities (parse input)
