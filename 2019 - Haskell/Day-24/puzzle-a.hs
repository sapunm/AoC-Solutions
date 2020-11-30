import Data.Bits
import qualified Data.Map.Strict as M
import Debug.Trace

parse :: String -> Int
parse input = foldl (\acc (i, c) -> if c == '#' then acc + 2^i else acc) 0 $ zip [0..] $ filter (/='\n') input

bitsToTest i = concat [top, left, right, bottom]
    where left = if i `mod` 5 > 0 then [i - 1] else []
          right = if i `mod` 5 < 4 then [i + 1] else []
          top = if i > 4 then [i - 5] else []
          bottom = if i < 20 then [i + 5] else []

countAdjBugs input i = foldl (\acc b -> if testBit input b then acc + 1 else acc) 0 $ bitsToTest i

timeStep :: Int -> Int
timeStep input = foldl getBug 0 [0..24]
    where getBug acc idx = if nextValue (testBit input idx) $ countAdjBugs input idx
                           then setBit acc idx
                           else acc
          nextValue True cnt = cnt == 1
          nextValue False cnt = cnt == 1 || cnt == 2

evolute input = evolute' input M.empty
        where evolute' input seen = if input `M.member` seen
                                    then input
                                    else evolute' (timeStep input) (M.insert input True seen)



printTiles input = (map.map) (\idx -> if testBit input idx then '#' else '.') [[0..4], [5..9], [10..14], [15..19], [20..24]]

--main :: IO ()
main = do
    input <- fmap parse getContents
    --print $ showIntAtBase 2 intToDigit input ""
    --mapM print $ map (countAdjBugs input) [0..24]
    print $ evolute input

