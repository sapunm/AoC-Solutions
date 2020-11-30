-- import Data.List (nub)
import qualified Data.Set as S
import qualified Data.HashTable.IO as H
import qualified Data.Array.IO as A
import qualified Data.Array.Repa as R

import Control.Monad.ST
import Data.STRef
import Control.Monad

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

transpose:: [[a]]->[[a]]
transpose ([]:_) = []
transpose x = (map head x) : transpose (map tail x)

parse :: String -> [[Int]]
parse input = map (\ line -> map read $ words [if c == ',' then ' ' else c | c <- line]) $ lines input

calcPositions :: [[Int]] -> [[Int]] -> [[Int]]
calcPositions positions velocities = map (\(p, v) -> ((\m -> map sum m) . transpose) [p, v]) $ zip positions velocities

calcVelocitiesDelta :: [[Int]] -> [[Int]]
calcVelocitiesDelta positions = map ((\m -> map sum m) . transpose) $ splitEvery 3 [ let (p1,p2) = (snd m1,snd m2) in map (\(x, y) -> signum (y - x)) (zip p1 p2) | m1 <- moons, m2 <- moons, fst m1 /= fst m2]
    where moons = zip [1..] positions

calcVelocities :: [[Int]] -> [[Int]] -> [[Int]]
calcVelocities positions velocities = map (\(p, v) -> ((\m -> map sum m) . transpose) [p, v]) $ zip delta velocities
    where delta = calcVelocitiesDelta positions

step (positions, velocities) = ((calcPositions positions newVelocities), newVelocities)
    where newVelocities = calcVelocities positions velocities

simulation currentStep = currentStep : simulation nextSep
        where nextSep = step currentStep

findPeriod currentStep states
    | S.member currentStep states = S.size states
    | otherwise = findPeriod (step currentStep) (S.insert currentStep states)

findPeriod' match count currentStep
    | currentStep `seq` match `seq` currentStep == match = count
    | otherwise = let nextStep = step currentStep
                      nextCount = count + 1
                    in nextStep `seq` nextCount `seq` match `seq` findPeriod' match nextCount nextStep

    -- foldl' f z (x:xs) = 
    --     let z' = f z x in z' `seq` foldl' f z' xs

type HashTable = H.CuckooHashTable ([[Int]], [[Int]]) Bool

findPeriod'' :: HashTable -> Int -> ([[Int]], [[Int]]) -> IO Int
findPeriod'' steps count currentStep = do
    found <- H.lookup steps currentStep
    case found of
        Just True -> do return count
        Nothing -> do
            H.insert steps currentStep True
            findPeriod'' steps (count + 1) (step currentStep)

--     | found == Nothing
--         where found < H.lookup states currentValues
--  0 = return ()
-- loop v = do
--    putStrLn "x not yet 0, enter an adjustment"
--    a <- readLn
--    loop (v+a)

-- main :: IO ()
-- main = do
--     input <- getContents
--     steps <- H.new :: IO (HashTable)
--     period <- findPeriod'' steps 0 (parse input, replicate 4 [0, 0, 0])
--     print period
    -- print $ findPeriod (parse input, replicate 4 [0, 0, 0]) S.empty

findPeriodST ::  ([[Int]], [[Int]]) -> Int
findPeriodST input = runST $ do
    start <- newSTRef input
    current <- newSTRef (step input)
    count <- newSTRef 0
    findPeriodST' start current count

    where findPeriodST' :: STRef s ([[Int]], [[Int]]) -> STRef s ([[Int]], [[Int]]) -> STRef s Int -> ST s Int
          findPeriodST' start current count = do
              start' <- readSTRef start
              current' <- readSTRef current
              count' <- readSTRef count
              if start `seq` current `seq` count `seq` start' `seq` current' `seq` start' == current'
              then return (count')
              else do
                  writeSTRef count (let inc = count' + 1 in start `seq` current `seq` count `seq` count' `seq` inc)
                  writeSTRef current (let next = step current' in start `seq` current `seq` count `seq` current' `seq` next)
                  findPeriodST' start current count

-- fibST :: Integer -> Integer
-- fibST n = 
--     if n < 2
--     then n
--     else runST $ do
--         x <- newSTRef 0
--         y <- newSTRef 1
--         fibST' n x y

--     where fibST' 0 x _ = readSTRef x
--           fibST' n x y = do
--               x' <- readSTRef x
--               y' <- readSTRef y
--               writeSTRef x y'
--               writeSTRef y $! x'+y'
--               fibST' (n-1) x y

main = do
    input <- getContents
    print $ findPeriodST (parse input, replicate 4 [0, 0, 0])
    -- print $ fibST 1000000000
    -- print $ let start = (parse input, replicate 4 [0, 0, 0]) in findPeriod start S.empty