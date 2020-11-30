parse :: String -> [Int]
parse input = map read (words [if c == ',' then ' ' else c | c <- input])

directAddr :: [Int] -> Int -> Int
directAddr memory addr = memory !! addr

indirectAddr :: [Int] -> Int -> Int -> Int
indirectAddr memory addr base = memory !! ((directAddr memory addr) + base)

getParam :: [Int] -> Int -> Int -> Int -> Int
getParam memory addr mode base
    | mode == 0 = indirectAddr memory addr 0
    | mode == 1 = directAddr memory addr
    | mode == 2 = indirectAddr memory addr base

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

computeBinaryOp :: [Int] -> Int -> Int -> (Int -> Int -> Int) -> (Int, Int, Int, Int) -> [Int]
computeBinaryOp memory base ip op (_, mod1, mod2, mod3) = take saveTo memory ++ (leftHand  `op` rightHand) : drop (saveTo + 1) memory
    where (leftHand, rightHand, saveTo) = (getParam memory (ip + 1) mod1 base, getParam memory (ip + 2) mod2 base, getParam memory (ip + 3) 1 base + (if mod3 == 2 then base else 0))

input :: [Int] -> Int -> Int-> Int -> Int -> [Int]
input memory base ip mod value = take saveTo memory ++ (value : drop (saveTo + 1) memory)
    where saveTo = (getParam memory (ip + 1) 1 0) + (if mod == 2 then base else 0)

output :: [Int] -> Int -> Int -> (Int, Int, Int, Int) -> Int
output memory base ip (_, mod1, _, _) = getParam memory (ip + 1) mod1 base

jump :: Bool -> [Int] -> Int -> Int -> (Int, Int, Int, Int) -> Int
jump ifTrue memory base ip (_, mod1, mod2, _)
    | ifTrue = if (getParam memory (ip + 1) mod1 base) /= 0 then getParam memory (ip + 2) mod2 base else ip + 3
    | otherwise = if (getParam memory (ip + 1) mod1 base) == 0 then getParam memory (ip + 2) mod2 base else ip + 3

computeNextOutput :: [Int] -> Int -> Int -> [Int] -> Maybe (Int, [Int], Int, Int, [Int])
computeNextOutput memory base ip inputs
    | opCode == 1 = computeNextOutput (computeBinaryOp memory base ip (+) instruction) base (ip + 4) inputs  -- add
    | opCode == 2 = computeNextOutput (computeBinaryOp memory base ip (*) instruction) base (ip + 4) inputs  -- mul
    | opCode == 3 = computeNextOutput (input memory base ip mod1 (head inputs)) base (ip + 2) (drop 1 inputs)     -- input
    | opCode == 4 = Just (output memory base ip instruction, memory, base, ip + 2, inputs)              -- output
    | opCode == 5 = computeNextOutput memory base (jump True memory base ip instruction) inputs            -- jmp if true
    | opCode == 6 = computeNextOutput memory base (jump False memory base ip instruction) inputs           -- jmp if false
    | opCode == 7 = computeNextOutput (computeBinaryOp memory base ip (cmpOp (<)) instruction) base (ip + 4) inputs  -- less than
    | opCode == 8 = computeNextOutput (computeBinaryOp memory base ip (cmpOp (==)) instruction) base (ip + 4) inputs  -- equals
    | opCode == 9 = computeNextOutput memory (base + getParam memory (ip + 1) mod1 base) (ip + 2) inputs  -- change base addr
    | otherwise = Nothing
        where instruction@(opCode, mod1, _, _) = decodeInstruction memory ip

computeProgram :: [Int] -> Int -> Int -> [Int] -> [Int] -> [Int]
computeProgram memory base ip inputs outputs = case (computeNextOutput memory base ip inputs) of
    Nothing -> reverse outputs
    Just (output', memory', base', ip', inputs') -> computeProgram memory' base' ip' inputs' (output':outputs)

turn :: Int -> [a] -> [a]
turn 0 l = last l : init l
turn 1 (h:rest) = rest ++ [h]

getDirs = [(0, 1), (1, 0), (0, -1), (-1, 0)]

getInput :: (Int, Int) -> [((Int, Int), Int)] -> Int
getInput (x, y) state
    | null found = 0
    | otherwise = snd (head found)
        where found = filter ((==(x, y)) . fst) state

runRobot :: (Int, Int) -> [(Int, Int)] -> [Int] -> Int -> Int -> [Int] -> [((Int, Int), Int)] -> [((Int, Int), Int)]
runRobot pos@(x, y) dirs memory base ip inputs outputs = case (computeNextOutput memory base ip inputs) of
    Nothing -> reverse outputs
    Just (output', memory', base', ip', inputs') -> case (computeNextOutput memory' base' ip' inputs') of
        Nothing -> reverse outputs
        Just (output'', memory'', base'', ip'', inputs'') ->
            let
                newDirs = turn output'' dirs
                (dx, dy) = head newDirs
                newPos = (x + dx, y + dy)
            in
                runRobot newPos newDirs memory'' base'' ip'' [getInput newPos outputs] (((x, y), output'):outputs)

getCoords :: [((Int, Int), Int)] -> ((Int, Int), (Int, Int))
getCoords output = ((minimum xs, minimum ys), (maximum xs, maximum ys))
    where (xs, ys) = unzip $ map fst output

readOutput :: [((Int, Int), Int)] -> [String]
readOutput output = reverse [[if getInput (x, y) output == 1 then '#' else ' ' | x <- [minx..maxx]] | y <- [miny..maxy]]
    where ((minx, miny), (maxx, maxy)) = getCoords output

--main :: IO ()
main = do
    input <- getContents
    mapM print $ readOutput $ runRobot (0, 0) (getDirs) (parse input ++ replicate 128000 0) 0 0 [1] []
