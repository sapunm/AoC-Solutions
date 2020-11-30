import Data.List (transpose)

parse :: String -> [([Int], [Int])]
parse input = zip readPositions initVelocities
    where readPositions = transpose $! map (\line -> map read $ words [c | c <- line, not (c `elem` "xyz=,<>")]) $ lines input
          initVelocities = replicate 3 [0, 0, 0, 0]

step :: ([Int], [Int]) -> ([Int], [Int])
step (positions, velocities) = (zipWith (+) positions newVelocities, newVelocities)
    where newVelocities = zipWith (+) velocities deltaV
          deltaV = [sum  [signum (p2 - p1) | p2 <- positions] | p1 <- positions]

find1DPeriod :: ([Int], [Int]) -> Int -> ([Int], [Int]) -> Int
find1DPeriod match count currentStep
    | currentStep `seq` currentStep == match = count
    | otherwise = nextStep `seq` nextCount `seq` find1DPeriod match nextCount nextStep
        where nextStep = step currentStep
              nextCount = count + 1

find3DPeriod :: [([Int], [Int])] -> Int
find3DPeriod dimensions = foldl1 lcm periods
    where periods = map (\dimension -> find1DPeriod dimension 1 (step dimension)) dimensions

main = do
    input <- getContents
    print $ find3DPeriod (parse input)