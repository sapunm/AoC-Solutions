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

cmpOp :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
cmpOp op = \left right -> fromEnum $ left `op` right

computeBinaryOp :: [Int] -> Int -> (Int -> Int -> Int) -> (Int, Int, Int, Int) -> [Int]
computeBinaryOp memory ip op (_, mod1, mod2, mod3) = take saveTo memory ++ (leftHand  `op` rightHand) : drop (saveTo + 1) memory
    where (leftHand, rightHand, saveTo) = (getParam memory (ip + 1) mod1, getParam memory (ip + 2) mod2, getParam memory (ip + 3) 1)

input :: [Int] -> Int -> Int -> [Int]
input memory ip value = take saveTo memory ++ (value : drop (saveTo + 1) memory)
    where saveTo = getParam memory (ip + 1) 1

output :: [Int] -> Int -> (Int, Int, Int, Int) -> Int
output memory ip (_, mod1, _, _) = getParam memory (ip + 1) mod1

jump :: Bool -> [Int] -> Int -> (Int, Int, Int, Int) -> Int
jump ifTrue memory ip (_, mod1, mod2, _)
    | ifTrue = if (getParam memory (ip + 1) mod1) /= 0 then getParam memory (ip + 2) mod2 else ip + 3
    | otherwise = if (getParam memory (ip + 1) mod1) == 0 then getParam memory (ip + 2) mod2 else ip + 3

computeProgram :: [Int] -> Int -> [Int] -> [Int] -> [Int]
computeProgram memory ip inputs outputs
    | opCode == 1 = computeProgram (computeBinaryOp memory ip (+) instruction) (ip + 4) inputs outputs  -- add
    | opCode == 2 = computeProgram (computeBinaryOp memory ip (*) instruction) (ip + 4) inputs outputs  -- mul
    | opCode == 3 = computeProgram (input memory ip (head inputs)) (ip + 2) (drop 1 inputs) outputs     -- input
    | opCode == 4 = computeProgram (memory) (ip + 2) inputs ((output memory ip instruction) : outputs)  -- output
    | opCode == 5 = computeProgram (memory) (jump True memory ip instruction) inputs outputs            -- jmp if true
    | opCode == 6 = computeProgram (memory) (jump False memory ip instruction) inputs outputs           -- jmp if false
    | opCode == 7 = computeProgram (computeBinaryOp memory ip (cmpOp (<)) instruction) (ip + 4) inputs outputs  -- less than
    | opCode == 8 = computeProgram (computeBinaryOp memory ip (cmpOp (==)) instruction) (ip + 4) inputs outputs  -- equals
    | otherwise = reverse outputs
        where instruction@(opCode, _, _, _) = decodeInstruction memory ip

main :: IO ()
main = do
    input <- getContents
    print $ computeProgram (parse input) 0 [5] []
