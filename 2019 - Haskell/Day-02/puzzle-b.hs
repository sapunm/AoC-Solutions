parse :: String -> [Int]
parse input = map read (words [if c == ',' then ' ' else c | c <- input])

putInputNounAndVerb :: [Int] -> Int -> Int -> [Int]
putInputNounAndVerb (byte0:byte1:byte2:rest) noun verb = byte0 : noun : verb : rest 

indirectAddr :: [Int] -> Int -> Int
indirectAddr input addr = input !! (input !! addr)

computeOp :: [Int] -> Int -> (Int -> Int -> Int) -> [Int]
computeOp memory ip op = take saveTo memory ++ (leftHand  `op` rightHand) : drop (saveTo + 1) memory
    where (leftHand, rightHand, saveTo) = (indirectAddr memory (ip + 1), indirectAddr memory (ip + 2), memory !! (ip + 3))

computeProgram :: [Int] -> Int -> [Int]
computeProgram memory ip
    | opCode == 1 = computeProgram (computeOp memory ip (+)) (ip + 4)
    | opCode == 2 = computeProgram (computeOp memory ip (*)) (ip + 4)
    | otherwise = memory
        where opCode = memory !! ip

result :: [Int] -> Int -> Int -> (Int, Int)
result memory noun verb = (head (computeProgram (putInputNounAndVerb memory noun verb) 0), 100 * noun + verb)

searchForInputs :: [Int] -> [Int] -> [Int] -> Int -> [(Int, Int)]
searchForInputs memory nouns verbs expectedResult = takeWhileInclusive ((/=expectedResult) . (fst)) [result memory n v | n <- nouns, v <- verbs]

takeWhileInclusive :: (a -> Bool) -> [a] -> [a]
takeWhileInclusive _ [] = []
takeWhileInclusive p (x:xs) = x : if p x then takeWhileInclusive p xs else []

main :: IO ()
main = do
    input <- getContents
    print $ snd (last (searchForInputs (parse input) [0..99] [0..99] 19690720))
