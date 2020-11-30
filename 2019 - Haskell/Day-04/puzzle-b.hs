parse :: String -> [Int]
parse input = [head parsed .. last parsed] where parsed = map read $ words [if c == '-' then ' ' else c | c <- input]

hasDouble :: String -> Int -> Bool
hasDouble (c1:c2:rest) groupSize
    | c1 == c2 && null rest && groupSize == 1 = True
    | c1 /= c2 && groupSize == 2 = True
    | c1 == c2 = hasDouble (c2:rest) (groupSize + 1)
    | otherwise = hasDouble (c2:rest) 1
hasDouble [c] _ = False
hasDouble [] _ = False

isNotDecreasing :: String -> Bool
isNotDecreasing (c1:c2:rest) = if c1 > c2 then False else isNotDecreasing (c2:rest)
isNotDecreasing [c] = True
isNotDecreasing [] = True

isConforming :: String -> Bool
isConforming pass = hasDouble pass 1 && isNotDecreasing pass

countConformingPasswords :: [Int] -> Int
countConformingPasswords range = length $ filter isConforming $ map show range

main :: IO ()
main = do
    input <- getContents
    print $ countConformingPasswords $ parse input
