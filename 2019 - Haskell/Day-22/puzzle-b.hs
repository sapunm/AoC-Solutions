import Data.List
import qualified Data.Map.Strict as M
import Debug.Trace

parse :: String -> [[String]]
parse input = map words $ lines input

-- backwards Euclidean algorithm
modInverse _ 0 = error ("divide by divisor non-coprime to modulus")
modInverse _ 1 = (0, 1)
modInverse n x = (r', q' - r' * q)
  where
    (q,  r)  = n `quotRem` x
    (q', r') = modInverse x r

shuffler :: Integer -> [[String]] -> [Integer -> Integer]
shuffler deckSize techniquesList = shuffler' techniquesList []
    where dealIntoNewStack i = deckSize - 1 - i
          cutStack at i = (deckSize + at + i) `mod` deckSize    
          -- dealWithIncrement inc i = traceShow (inc, i) $ (let mult = head $ dropWhile ((/=i).(`mod` deckSize)) [j*inc | j <- [0..]] in traceShow (mult `div` inc) $ mult `div` inc)
          -- dealWithIncrement inc i = (let mult = head $ dropWhile ((/=i).(`mod` deckSize)) [j*inc | j <- [0..]] in mult `div` inc)
          dealWithIncrement inc i = ((snd $ modInverse deckSize inc) * i) `mod` deckSize-- (let mult = head $ dropWhile ((/=i).(`mod` deckSize)) [j*inc | j <- [0..]] in mult `div` inc)
          shuffler' [] funs = funs
          shuffler' (tech:rest) funs = case tech of
            [_, _, "increment", i] -> shuffler' rest $ (dealWithIncrement (read i ::Integer)):funs
            [_, _, "new", "stack"] -> shuffler' rest $ (dealIntoNewStack):funs
            ["cut", i] -> shuffler' rest $ (cutStack (read i  ::Integer)):funs

getCardAtIndex shuffler i = foldl' (\idx fun -> fun idx) i shuffler

findPeriod shuffler position = period (getCardAtIndex shuffler position) 1
    where period i cnt 
            | i == position = cnt
            | otherwise = period (getCardAtIndex shuffler i) (if cnt `mod` 10000000 == 0 then traceShow cnt $ cnt + 1 else cnt + 1)

--main :: IO ()
main = do
    input <- fmap parse getContents
    -- print $ getCardAtIndex (shuffler 119315717514047 input) 2020
    print $ findPeriod (shuffler 119315717514047 input) 2020
    -- print $ getCardAtIndex (shuffler 10007 input) 4485
