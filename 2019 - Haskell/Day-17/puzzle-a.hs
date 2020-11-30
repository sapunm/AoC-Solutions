import Data.Maybe (fromJust)
import Data.Either (fromLeft, fromRight)
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M
import Data.Map ((!), (!?))
import Data.Sequence ((<|), (|>), Seq((:<|)), Seq((:|>)))
import Debug.Trace

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

computeNOutputs :: Int -> [Int] -> Int -> Int -> [Int] -> [Int] -> Maybe ([Int], [Int], Int, Int, [Int])
computeNOutputs 0 memory base ip inputs outputs = Just (reverse outputs, memory, base, ip, inputs)
computeNOutputs n memory base ip inputs outputs = case (computeNextOutput memory base ip inputs) of
    Nothing -> Just (reverse outputs, memory, base, ip, inputs)
    Just (output', memory', base', ip', inputs') -> computeNOutputs (n-1) memory' base' ip' inputs' (output':outputs)

computeProgram :: [Int] -> Int -> Int -> [Int] -> [Int] -> [Int]
computeProgram memory base ip inputs outputs = case (computeNextOutput memory base ip inputs) of
    Nothing -> reverse outputs
    Just (output', memory', base', ip', inputs') -> computeProgram memory' base' ip' inputs' (output':outputs)

computeNextOutputInteractive :: [Int] -> Int -> Int -> IO Int -> IO (Maybe (Int, [Int], Int, Int))
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

computeNOutputsInteractive :: Int -> [Int] -> Int -> Int -> IO Int -> [Int] -> IO (Maybe ([Int], [Int], Int, Int))
computeNOutputsInteractive 0 memory base ip inputIO outputs = do return(Just (reverse outputs, memory, base, ip))
computeNOutputsInteractive n memory base ip inputIO outputs = do
    results <- computeNextOutputInteractive memory base ip inputIO
    case results of
        Nothing -> return (Just (reverse outputs, memory, base, ip))
        Just (output', memory', base', ip') -> computeNOutputsInteractive (n-1) memory' base' ip' inputIO (output':outputs)

type Pos = (Int, Int)
type Distance = Int
type Move = ((Int, Int), [Int])
type ProgramState = (Int, [Int], Int, Int)
type VisitedMap = M.Map Pos Int
type RobotState = (Distance, Pos, ProgramState)
type StateQueue = S.Seq RobotState

moves :: [Move]
moves = [((0, 1), [1]), ((0, -1), [2]), ((-1, 0), [3]), ((1, 0), [4])]

addPos :: Pos -> Pos -> Pos
addPos (x, y) (x', y') = (x + x', y + y')

mapMove :: Pos -> Move ->  Move
mapMove (x, y) ((x', y'), z) = ((x + x', y + y'), z)

visited :: VisitedMap -> Either Pos Move -> Bool
visited visitedMap (Left pos) = pos `M.member` visitedMap
visited visitedMap (Right (pos, _)) = pos `M.member` visitedMap

-- visitNeigbors :: Pos -> VisitedMap -> ([Int], Int, Int) -> 
visitNeigbors pos visitedMap (mem, base, ip) =  map (fmap (fromJust . computeNextOutput mem base ip)) $ filter (not . visited visitedMap . Right . mapMove pos) moves

processVisited (dist, pos, _) (q, visitedMap) (dp, (out, mem, base, ip, inputs))
  | out == 0 = (q, M.insert (addPos pos dp) out visitedMap)
  | out == 1 = let newPos = addPos pos dp in (q |> (dist + 1, newPos, (out, mem, base, ip)), M.insert newPos out visitedMap)
  | out == 2 = let newPos = addPos pos dp in ((dist + 1, newPos, (out, mem, base, ip)) <| q, M.insert newPos out visitedMap)

bfsSearch (S.Empty, _) = Nothing 
bfsSearch ((curr@(dist, pos, (out, mem, base, ip)) :<| q), visitedMap)
  | out == 2 = Just dist
  | otherwise = bfsSearch $! foldl (processVisited curr) (q, visitedMap) $ visitNeigbors pos visitedMap (mem, base, ip)

shortestPath :: [Int] -> Maybe Distance
shortestPath program = bfsSearch ((S.singleton (0, (0, 0), (1, program, 0, 0))), M.singleton (0, 0) 1)

bfsTraverse (S.Empty, visitedMap) = visitedMap 
bfsTraverse ((curr@(dist, pos, (out, mem, base, ip)) :<| q), visitedMap) = bfsTraverse $! foldl (processVisited curr) (q, visitedMap) $ visitNeigbors pos visitedMap (mem, base, ip)

splitOn list val = getOne list []
   where getOne [] listOfLists = reverse $ filter (/=[]) listOfLists
         getOne list listOfLists = getOne (drop 1 $ dropWhile (/=val) list) $ (takeWhile (/=val) list) : listOfLists 

mapOutput 35 = '#'
mapOutput 46 = ' '
mapOutput 60 = '<'
mapOutput 62 = '>'
mapOutput 76 = 'v'
mapOutput 94 = '^'

getCameraView program = map (map mapOutput) $ splitOn outputs 10
   where outputs = computeProgram program 0 0 [] []

getScaffoldingMap :: [Int] -> VisitedMap
getScaffoldingMap program = foldl (updateMap) M.empty $ filter ((/=46) . snd) $ concat coordinates
   where coordinates = map (\(y, row) -> map (\(x, e) -> ((x, y), e)) $ zip [0..] row) $ zip [0..] $ splitOn outputs 10
         outputs = computeProgram program 0 0 [] []
         updateMap m (pos, v) = M.insert pos v m

getScaffoldingAlignment :: [Int] -> Distance
getScaffoldingAlignment program = sum $ map (getAlignment) $ M.toList scaffMap
   where scaffMap = getScaffoldingMap program
         isIntersection pos = (>=3) $ length $ filter (visited scaffMap . Right . mapMove pos) moves
         getAlignment (pos@(x, y), _) = if isIntersection pos then x*y else 0 

--main :: IO ()
main = do
    input <- fmap parse getContents
    program <- return $ input ++ replicate 8192 0
    print $ getScaffoldingAlignment program
