parse :: String -> [Int]
parse input = [head parsed .. last parsed] where parsed = map read $ words [if c == '-' then ' ' else c | c <- input]

hasDouble :: String -> Bool
hasDouble (c1:c2:rest) = if c1 == c2 then True else hasDouble(c2:rest)
hasDouble [c] = False
hasDouble [] = False

isNotDecreasing :: String -> Bool
isNotDecreasing (c1:c2:rest) = if c1 > c2 then False else isNotDecreasing(c2:rest)
isNotDecreasing [c] = True
isNotDecreasing [] = True

isConforming :: String -> Bool
isConforming pass = hasDouble pass && isNotDecreasing pass

countConformingPasswords :: [Int] -> Int
countConformingPasswords range = length $ filter isConforming $ map show range

main :: IO ()
main = do
    input <- getContents
    print $ countConformingPasswords $ parse input
