parse :: String -> [Int]
parse input = map read (words [if c == ',' then ' ' else c | c <- input])

directAddr :: [Int] -> Int -> Int
directAddr memory addr = memory !! addr

indirectAddr :: [Int] -> Int -> Int
indirectAddr memory addr = memory !! (directAddr memory addr)

getParam :: [Int] -> Int -> Int -> Int
getParam memory addr mode
    | mode == 0 = indirectAddr memory addr
    | mode == 1 = directAddr memory addr

paddWithZeros :: String -> String
paddWithZeros instruction = replicate (5 - length instruction) '0' ++ instruction

decodeInstruction :: [Int] -> Int -> (Int, Int, Int, Int)
decodeInstruction memory ip = (
        read (drop 3 strCode),
        read [strCode !! 2],
        read [strCode !! 1],
        read [strCode !! 0]
    )
    where strCode = paddWithZeros . show $ memory !! ip

computeBinaryOp :: [Int] -> Int -> (Int -> Int -> Int) -> (Int, Int, Int, Int) -> [Int]
computeBinaryOp memory ip op (_, mod1, mod2, mod3) = take saveTo memory ++ (leftHand  `op` rightHand) : drop (saveTo + 1) memory
    where (leftHand, rightHand, saveTo) = (getParam memory (ip + 1) mod1, getParam memory (ip + 2) mod2, getParam memory (ip + 3) 1)

input :: [Int] -> Int -> Int -> [Int]
input memory ip value = take saveTo memory ++ (value : drop (saveTo + 1) memory)
    where saveTo = getParam memory (ip + 1) 1

output :: [Int] -> Int -> (Int, Int, Int, Int) -> Int
output memory ip (_, mod1, _, _) = getParam memory (ip + 1) mod1

computeProgram :: [Int] -> Int -> [Int] -> [Int] -> [Int]
computeProgram memory ip inputs outputs
    | opCode == 1 = computeProgram (computeBinaryOp memory ip (+) instruction) (ip + 4) inputs outputs
    | opCode == 2 = computeProgram (computeBinaryOp memory ip (*) instruction) (ip + 4) inputs outputs
    | opCode == 3 = computeProgram (input memory ip (head inputs)) (ip + 2) (drop 1 inputs) outputs
    | opCode == 4 = computeProgram (memory) (ip + 2) inputs ((output memory ip instruction) : outputs)
    | otherwise = reverse outputs
        where instruction@(opCode, _, _, _) = decodeInstruction memory ip

main :: IO ()
main = do
    input <- getContents
    print $ computeProgram (parse input) 0 [1] []
