import Data.Bits
import qualified Data.Map.Strict as M
import Debug.Trace

parse :: String -> M.Map (Int, Int) Bool
parse input = foldl (\acc (i, c) -> if c == '#' then M.insert (0, i) True acc else acc) M.empty $ zip [1..] $ filter (/='\n') input

bitsToTest (lvl, i) = concat [left, top, right, bottom]
    where left
               | i == 14 = [(lvl + 1, i') | i' <- [5,10..25]]
               | i `mod` 5 == 1 = [(lvl - 1, 12)]
               | otherwise = [(lvl, i - 1)]
          right
               | i == 12 = [(lvl + 1, i') | i' <- [1,6..21]]
               | i `mod` 5 == 0 = [(lvl - 1, 14)]
               | otherwise = [(lvl, i + 1)]
          top
               | i == 18 = [(lvl + 1, i') | i' <- [21..25]]
               | i <= 5 = [(lvl - 1, 8)]
               | otherwise = [(lvl, i - 5)]
          bottom
               | i == 8 = [(lvl + 1, i') | i' <- [1..5]]
               | i > 20 = [(lvl - 1, 18)]
               | otherwise = [(lvl, i + 5)]

countAdjBugs input (lvl, i) = foldl (\acc b -> if b `M.member` input then acc + 1 else acc) 0 $ bitsToTest (lvl, i)

timeStep :: M.Map (Int, Int) Bool -> Int -> M.Map (Int, Int) Bool
timeStep input lvl = foldl getBug M.empty $ [(l, i) | l <- [(-lvl)..lvl], i <- [1..12] ++ [14..25]]
    where getBug acc idx = if nextValue (idx `M.member` input) $ countAdjBugs input idx
                           then M.insert idx True acc
                           else acc
          nextValue True cnt = cnt == 1
          nextValue False cnt = cnt == 1 || cnt == 2

evolute input steps = evolute' input steps 0
        where evolute' input 0 lvl = input
              evolute' input steps lvl = evolute' (timeStep input (lvl + 1)) (steps - 1) (lvl + 1)

printTiles input = (map.map) (\idx -> if testBit input idx then '#' else '.') [[0..4], [5..9], [10..14], [15..19], [20..24]]

--main :: IO ()
main = do
    input <- fmap parse getContents
    --print $ showIntAtBase 2 intToDigit input ""
    --mapM print $ map (countAdjBugs input) [0..24]
    print $ M.size $ evolute input 200

