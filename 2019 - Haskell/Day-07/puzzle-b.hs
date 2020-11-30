import Data.List (permutations)

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

computeNextOutput :: [Int] -> Int -> [Int] -> Maybe (Int, [Int], Int, [Int])
computeNextOutput memory ip inputs
    | opCode == 1 = computeNextOutput (computeBinaryOp memory ip (+) instruction) (ip + 4) inputs  -- add
    | opCode == 2 = computeNextOutput (computeBinaryOp memory ip (*) instruction) (ip + 4) inputs  -- mul
    | opCode == 3 = computeNextOutput (input memory ip (head inputs)) (ip + 2) (drop 1 inputs)     -- input
    | opCode == 4 = Just (output memory ip instruction, memory, ip + 2, inputs)                    -- output
    | opCode == 5 = computeNextOutput (memory) (jump True memory ip instruction) inputs            -- jmp if true
    | opCode == 6 = computeNextOutput (memory) (jump False memory ip instruction) inputs           -- jmp if false
    | opCode == 7 = computeNextOutput (computeBinaryOp memory ip (cmpOp (<)) instruction) (ip + 4) inputs  -- less than
    | opCode == 8 = computeNextOutput (computeBinaryOp memory ip (cmpOp (==)) instruction) (ip + 4) inputs  -- equals
    | otherwise = Nothing
        where instruction@(opCode, _, _, _) = decodeInstruction memory ip

computeProgram :: [Int] -> Int -> [Int] -> [Int] -> [Int]
computeProgram memory ip inputs outputs = case (computeNextOutput memory ip inputs) of
    Nothing -> reverse outputs
    Just (output', memory', ip', inputs') -> computeProgram memory' ip' inputs' (output':outputs)

getAmpsInitialState :: [Int] -> [Int] -> [([Int], Int, [Int])]
getAmpsInitialState ampControllerProgram phaseSettings = map (\phase -> (ampControllerProgram, 0, [phase])) phaseSettings

rotateByOne :: [a] -> [a]
rotateByOne = (drop <> take) 1

computeThrust :: [([Int], Int, [Int])] -> Int -> Int
computeThrust amplifiersState ampInput = case (computeNextOutput memory ip (rotateByOne (ampInput:inputs))) of
    Nothing -> ampInput
    Just (output', memory', ip', inputs') -> computeThrust (rotateByOne ((memory', ip', inputs'):(drop 1 amplifiersState))) output'
    where (memory, ip, inputs) = head amplifiersState

maxThrust :: [Int] -> Int
maxThrust ampControllerProgram = maximum $ map (\phase -> computeThrust (getAmpsInitialState ampControllerProgram phase) 0) $ permutations [5..9]

main :: IO ()
main = do
    input <- getContents
    print $ maxThrust (parse input)
