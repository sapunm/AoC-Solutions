parse :: String -> [Int]
parse input = map read (words [if c == ',' then ' ' else c | c <- input])

restore1202 :: [Int] -> [Int]
restore1202 (byte0:byte1:byte2:rest) = byte0 : 12 : 2 : rest 

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

main :: IO ()
main = do
    input <- getContents
    print $ head (computeProgram (restore1202 (parse input)) 0)
