import Data.Char (ord, chr)
import Data.List (groupBy, transpose, nub, sort, stripPrefix, intersperse, intercalate, isPrefixOf)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Either (fromLeft, fromRight)
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M
import Data.Map ((!), (!?))
import Data.Sequence ((<|), (|>), Seq((:<|)), Seq((:|>)))
import Debug.Trace

parse :: String -> M.Map Int Int
parse input = M.fromAscList $ zip [0..] program
    where program = map read (words [if c == ',' then ' ' else c | c <- input])

directAddr :: M.Map Int Int -> Int -> Int
directAddr memory addr = M.findWithDefault 0 addr memory

indirectAddr :: M.Map Int Int -> Int -> Int -> Int
indirectAddr memory addr base = M.findWithDefault 0 ((directAddr memory addr) + base) memory

getParam :: M.Map Int Int -> Int -> Int -> Int -> Int
getParam memory addr mode base
    | mode == 0 = indirectAddr memory addr 0
    | mode == 1 = directAddr memory addr
    | mode == 2 = indirectAddr memory addr base

paddWithZeros :: String -> String
paddWithZeros instruction = replicate (5 - length instruction) '0' ++ instruction

decodeInstruction :: M.Map Int Int -> Int -> (Int, Int, Int, Int)
decodeInstruction memory ip = (
        read (drop 3 strCode),
        read [strCode !! 2],
        read [strCode !! 1],
        read [strCode !! 0]
    )
    where strCode = paddWithZeros . show $ M.findWithDefault 0 ip memory

cmpOp :: (Int -> Int -> Bool) -> (Int -> Int -> Int)
cmpOp op = \left right -> fromEnum $ left `op` right

computeBinaryOp :: M.Map Int Int -> Int -> Int -> (Int -> Int -> Int) -> (Int, Int, Int, Int) -> M.Map Int Int
computeBinaryOp memory base ip op (_, mod1, mod2, mod3) = M.insert saveTo (leftHand  `op` rightHand) memory
    where leftHand = getParam memory (ip + 1) mod1 base
          rightHand = getParam memory (ip + 2) mod2 base
          saveTo = getParam memory (ip + 3) 1 base + (if mod3 == 2 then base else 0)

input :: M.Map Int Int -> Int -> Int-> Int -> Int -> M.Map Int Int
input memory base ip mod value = M.insert saveTo value memory
    where saveTo = (getParam memory (ip + 1) 1 0) + (if mod == 2 then base else 0)

output :: M.Map Int Int -> Int -> Int -> (Int, Int, Int, Int) -> Int
output memory base ip (_, mod1, _, _) = getParam memory (ip + 1) mod1 base

jump :: Bool -> M.Map Int Int -> Int -> Int -> (Int, Int, Int, Int) -> Int
jump ifTrue memory base ip (_, mod1, mod2, _)
    | ifTrue = if (getParam memory (ip + 1) mod1 base) /= 0 then getParam memory (ip + 2) mod2 base else ip + 3
    | otherwise = if (getParam memory (ip + 1) mod1 base) == 0 then getParam memory (ip + 2) mod2 base else ip + 3

data ComputationResult = Halt | TicksElapsed (M.Map Int Int, Int, Int, [Int]) | Output (Int, M.Map Int Int, Int, Int, [Int]) deriving (Eq, Ord, Show)

computeNextOutput :: Int -> M.Map Int Int -> Int -> Int -> [Int] -> ComputationResult
computeNextOutput ticks memory base ip inputs
    | ticks == 0 = TicksElapsed (memory, base, ip, inputs) 
    | opCode == 1 = computeNextOutput (ticks - 1) (computeBinaryOp memory base ip (+) instruction) base (ip + 4) inputs  -- add
    | opCode == 2 = computeNextOutput (ticks - 1) (computeBinaryOp memory base ip (*) instruction) base (ip + 4) inputs  -- mul
    | opCode == 3 = computeNextOutput (ticks - 1) (input memory base ip mod1 (head inputs)) base (ip + 2) (drop 1 inputs)     -- input
    | opCode == 4 = Output (output memory base ip instruction, memory, base, ip + 2, inputs)              -- output
    | opCode == 5 = computeNextOutput (ticks - 1) memory base (jump True memory base ip instruction) inputs            -- jmp if true
    | opCode == 6 = computeNextOutput (ticks - 1) memory base (jump False memory base ip instruction) inputs           -- jmp if false
    | opCode == 7 = computeNextOutput (ticks - 1) (computeBinaryOp memory base ip (cmpOp (<)) instruction) base (ip + 4) inputs  -- less than
    | opCode == 8 = computeNextOutput (ticks - 1) (computeBinaryOp memory base ip (cmpOp (==)) instruction) base (ip + 4) inputs  -- equals
    | opCode == 9 = computeNextOutput (ticks - 1) memory (base + getParam memory (ip + 1) mod1 base) (ip + 2) inputs  -- change base addr
    | otherwise = Halt
        where instruction@(opCode, mod1, _, _) = decodeInstruction memory ip

computeNOutputs :: Int -> Int -> M.Map Int Int -> Int -> Int -> [Int] -> [Int] -> Maybe ([Int], M.Map Int Int, Int, Int, [Int])
computeNOutputs 0 ticks memory base ip inputs outputs = Just (reverse outputs, memory, base, ip, inputs)
computeNOutputs n ticks memory base ip inputs outputs = case (computeNextOutput ticks memory base ip inputs) of
    Halt -> Just (reverse outputs, memory, base, ip, inputs)
    TicksElapsed (memory', base', ip', inputs') -> Just (reverse outputs, memory', base', ip', inputs')
    Output (output', memory', base', ip', inputs') -> computeNOutputs (n-1) (ticks-1) memory' base' ip' inputs' (output':outputs)

computeOutputsUntil :: [Int] -> M.Map Int Int -> Int -> Int -> [Int] -> [Int] -> Maybe ([Int], M.Map Int Int, Int, Int, [Int])
computeOutputsUntil match memory base ip inputs outputs
    | isPrefixOf revMatch outputs = Just (reverse outputs, memory, base, ip, inputs)
    | otherwise = case computeNextOutput (2^20) memory base ip inputs of
        Halt -> Just (reverse outputs, memory, base, ip, inputs)
        TicksElapsed (memory', base', ip', inputs') -> computeOutputsUntil match memory' base' ip' inputs' outputs
        Output (output', memory', base', ip', inputs') -> computeOutputsUntil match memory' base' ip' inputs' (output':outputs)
    where revMatch = reverse match

computeProgram :: M.Map Int Int -> Int -> Int -> [Int] -> [Int] -> [Int]
computeProgram memory base ip inputs outputs = case (computeNextOutput (2^20) memory base ip inputs) of
    Halt -> reverse outputs
    TicksElapsed (memory', base', ip', inputs') -> computeProgram memory' base' ip' inputs' outputs
    Output (output', memory', base', ip', inputs') -> computeProgram memory' base' ip' inputs' (output':outputs)

computeNextOutputInteractive :: M.Map Int Int -> Int -> Int -> IO Int -> IO (Maybe (Int, M.Map Int Int, Int, Int))
computeNextOutputInteractive memory base ip inputIO
    | opCode == 1 = computeNextOutputInteractive (computeBinaryOp memory base ip (+) instruction) base (ip + 4) inputIO  -- add
    | opCode == 2 = computeNextOutputInteractive (computeBinaryOp memory base ip (*) instruction) base (ip + 4) inputIO  -- mul
    | opCode == 3 = do
        value <- inputIO
        computeNextOutputInteractive (input memory base ip mod1 value) base (ip + 2) inputIO    -- input
    | opCode == 4 = return (Just (output memory base ip instruction, memory, base, ip + 2))              -- output
    | opCode == 5 = computeNextOutputInteractive memory base (jump True memory base ip instruction) inputIO            -- jmp if true
    | opCode == 6 = computeNextOutputInteractive memory base (jump False memory base ip instruction) inputIO           -- jmp if false
    | opCode == 7 = computeNextOutputInteractive (computeBinaryOp memory base ip (cmpOp (<)) instruction) base (ip + 4) inputIO  -- less than
    | opCode == 8 = computeNextOutputInteractive (computeBinaryOp memory base ip (cmpOp (==)) instruction) base (ip + 4) inputIO  -- equals
    | opCode == 9 = computeNextOutputInteractive memory (base + getParam memory (ip + 1) mod1 base) (ip + 2) inputIO  -- change base addr
    | otherwise = return Nothing
        where instruction@(opCode, mod1, _, _) = decodeInstruction memory ip

computeNOutputsInteractive :: Int -> M.Map Int Int -> Int -> Int -> IO Int -> [Int] -> IO (Maybe ([Int], M.Map Int Int, Int, Int))
computeNOutputsInteractive 0 memory base ip inputIO outputs = do return(Just (reverse outputs, memory, base, ip))
computeNOutputsInteractive n memory base ip inputIO outputs = do
    results <- computeNextOutputInteractive memory base ip inputIO
    case results of
        Nothing -> return (Just (reverse outputs, memory, base, ip))
        Just (output', memory', base', ip') -> computeNOutputsInteractive (n-1) memory' base' ip' inputIO (output':outputs)

asciiProgram = map ord $ concat progText
    where progText = ["NOT H T\n",
                      "AND A T\n",
                      "AND B T\n",
                      "AND C T\n",
                      "NOT T J\n",
                      "AND D J\n",
                      "RUN\n"]

validInstructions numReg = [ concat [op, r1, r2]  | op <- ops, r1 <- firstReg, r2 <- secReg]
    where ops = ["OR ", "AND ", "NOT "]
          firstReg = map ((++ " ") . (:[])) $ (take numReg ['A'..'I']) ++ ['T', 'J']
          secReg = ["T\n", "J\n"]

splitOn list val = getOne list []
   where getOne [] listOfLists = reverse $ filter (/=[]) listOfLists
         getOne list listOfLists = getOne (drop 1 $ dropWhile (/=val) list) $ (takeWhile (/=val) list) : listOfLists 

getCameraView outputs = map (map mapOutput) $ splitOn outputs 10
    where mapOutput = chr

--main :: IO ()
main = do
    input <- fmap parse getContents
    output <- return $ computeProgram input 0 0 asciiProgram []
    --print $ length $ validInstructions 8
    --return ()
    mapM print $ getCameraView $ takeWhile (<128) output
    print $ dropWhile (<128) output
