import Data.Char (ord, chr)
import Data.List (groupBy, transpose, nub, sort, stripPrefix, intersperse, intercalate)
import Data.Maybe (fromJust, isJust, isNothing)
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

subPos :: Pos -> Pos -> Pos
subPos (x, y) (x', y') = (x - x', y - y')

dist :: Pos -> Pos -> Distance
dist (x, y) (x', y') = abs(x' - x) + abs(y' - y)

mapMove :: Pos -> Move ->  Move
mapMove (x, y) ((x', y'), z) = ((x + x', y + y'), z)

visited :: VisitedMap -> Either Pos Move -> Bool
visited visitedMap (Left pos) = pos `M.member` visitedMap
visited visitedMap (Right (pos, _)) = pos `M.member` visitedMap

splitOn list val = getOne list []
   where getOne [] listOfLists = reverse $ filter (/=[]) listOfLists
         getOne list listOfLists = getOne (drop 1 $ dropWhile (/=val) list) $ (takeWhile (/=val) list) : listOfLists 

getCameraView outputs = map (map mapOutput) $ splitOn outputs 10
    where mapOutput = chr

getScaffoldingMap :: [Int] -> VisitedMap
getScaffoldingMap outputs = foldl (updateMap) M.empty $ filter ((/=46) . snd) $ concat coordinates
   where coordinates = map (\(y, row) -> map (\(x, e) -> ((x, y), e)) $ zip [0..] row) $ zip [0..] $ splitOn outputs 10
         updateMap m (pos, v) = M.insert pos v m

getScaffoldingAlignment :: VisitedMap -> Distance
getScaffoldingAlignment scaffMap = sum $ map (getAlignment) $ M.toList scaffMap
   where getAlignment (pos@(x, y), _) = if isIntersection scaffMap pos then x*y else 0 

isIntersection scaffMap pos = (>=3) $ length $ filter (visited scaffMap . Right . mapMove pos) moves

data EdgeLabel = North | South | West | East deriving (Eq, Ord, Show, Read, Enum)
type Node = Pos
type VisitedEdges = M.Map (Node, Node) Bool
type Edges = M.Map (Node, Node) EdgeLabel
type Nodes = M.Map Node [Node]
type ScaffGraph = (Nodes, Edges)
type Path = [Node]
type PathQueue = S.Seq (Path, VisitedEdges)

getScaffoldingGraph :: [Int] -> VisitedMap -> ScaffGraph
getScaffoldingGraph outputs scaffMap = (foldl (\m ((a, b),_) -> M.insertWith (++) a [b] m) M.empty $ M.toList edges, edges)
    where edges = (foldColumns . foldRows) M.empty
          foldRows graph = foldl (foldRowCol East West) graph rows
          foldColumns graph = foldl (foldRowCol South North) graph columns
          foldRowCol l1 l2 graph col = foldl (\gr e@(a,b) -> M.insert e l1 $ M.insert (b, a) l2 gr) graph $ zip col $ tail col
          columns = concat $ filter (/=[]) $ map (nodes . nonEmpty . groups) $ transpose lines
          rows = concat $ filter (/=[]) $ map (nodes . nonEmpty . groups) lines
          nodes nonEmpty = map (\(x:xs) -> x : filter (isIntersection scaffMap) xs ++ [last xs]) nonEmpty
          nonEmpty groups = (map . map) (fst) $ filter ((>1) . length) $ filter ((/=46).snd.head) groups
          groups line = groupBy (\(_, c) (_, c') -> if c == (46 ::Int) then c == c' else c' /= (46 ::Int)) line
          lines = map (\(y, row) -> map (\(x, e) -> ((x, y), e)) $ zip [0..] row) $ zip [0..] $ splitOn outputs 10


getEulerianPaths :: Node -> ScaffGraph -> [Path]
getEulerianPaths from (nodes, edges) = getEulerianPaths' ((S.singleton ([from], M.empty :: VisitedEdges)), [])
    where getEulerianPaths' (S.Empty, paths) = map reverse paths
          getEulerianPaths' (((path@(node:_), visited) :<| q), paths) =
            let neighbors = filter (\n' -> (node, n') `M.notMember` visited) $ nodes ! node
                markVisited a b = M.insert (a, b) True . M.insert (b, a) True
                visit (q, paths) node' =
                    if M.size edges `div` 2 == (length path)
                    then (q, (node':path):paths)
                    else ((node':path, markVisited node node' visited) <| q, paths)
            in getEulerianPaths' $! foldl visit (q, paths) $ neighbors

data Turn = L | R deriving (Eq, Ord, Show, Read, Enum)
type PathIntructions = [(Turn, Distance)]

turns = M.fromList [((North, West), L),
                    ((North, East), R),
                    ((South, West), R),
                    ((South, East), L),
                    ((West, North), R),
                    ((West, South), L),
                    ((East, North), L),
                    ((East, South), R)]

encodePath :: Edges -> Path-> PathIntructions
encodePath edges path = reverse $ encodePath' path (L, 0) []
    where encodePath' (n1:n2:n3:rest) (ct, cd) instuctions =
                      let e1 = edges ! (n1, n2)
                          e2 = edges ! (n2, n3)
                      in case turns !? (e1, e2) of
                         Just t -> encodePath' (n2:n3:rest) (t, 0) ((ct, cd + dist n1 n2):instuctions)
                         Nothing -> encodePath' (n2:n3:rest) (ct, cd + dist n1 n2) instuctions
          encodePath' (n1:n2:[]) (ct, cd) instuctions = ((ct, cd + dist n1 n2):instuctions)


--main :: IO ()
main = do
    input <- fmap parse getContents
    program <- return $ input ++ replicate 8192 0
    output <- return $ runTheDrone program
    mapM print $ getCameraView $ init output
    print $ last output

runTheDrone program = computeProgram (2:(tail program)) 0 0 (prepareProgramInput routine) []
    where routine = fromJust $ head $ dropWhile isNothing $ map (findRoutines . encodePath (snd scaffGr)) paths
          paths = getEulerianPaths (4, 0) scaffGr
          scaffGr = getScaffoldingGraph outputs scaffMap
          scaffMap = getScaffoldingMap outputs
          outputs = computeProgram program 0 0 [] []

prepareProgramInput :: (String, [PathIntructions]) -> [Int]
prepareProgramInput routine = intercalate [ord '\n'] [encodedRoutine, encodedFuns, [ord 'n', ord '\n']]
    where encodedRoutine = map ord $ intersperse ',' $ fst routine
          encodedFuns = intercalate [ord '\n'] $ map encodeFun $ snd routine
          encodeFun fun = map ord $ intercalate [','] $ map (\(x, y) -> ((head.show) x):',':(truncateStep y)) fun
          truncateStep step = show step


findRoutines path = findCommands2 [] [[head path]] 'A' path

incF funId = chr $ (ord funId) + 1

findCommands2 routine funs topF path
    | length funs > 3 || length routine > 10 || maximum (map length funs) > 5 = Nothing
    | null path = Just (reverse routine, funs)
    | otherwise = case getPrefix funs path of
        Just (whichFun, Just path') ->case findCommands2 (whichFun:routine) funs topF path' of
                                  Just ret -> Just ret
                                  Nothing -> case findCommands2 routine (funs ++ [[head path']]) (incF topF) path' of
                                      Just ret -> Just ret
                                      Nothing -> if null routine || topF `notElem` routine
                                          then findCommands2 routine (init funs ++ [last funs ++ [head path']]) topF path
                                          else  Nothing
        Nothing -> case findCommands2 routine (funs ++ [[head path]]) (incF topF) path of
                   Just ret -> Just ret
                   Nothing -> Nothing 

getPrefix funs path = if null applyFuns then Nothing else Just (head applyFuns)
    where applyFuns = dropWhile (isNothing . snd) $ zip ['A'..] $ map (\k -> stripPrefix k path) funs
