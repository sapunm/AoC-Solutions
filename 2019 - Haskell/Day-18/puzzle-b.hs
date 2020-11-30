import Data.List (sort)
import Data.Char (ord, chr, toLower, isLower, isUpper)
import Data.Maybe (fromJust)
import Data.Either (fromLeft, fromRight)
import Data.IORef
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M
import Data.Map ((!), (!?))
import Data.Sequence ((<|), (|>), Seq((:<|)), Seq((:|>)))
import Data.Bits
import Debug.Trace
import Control.Parallel (par, pseq)

type Distance = Int
type Pos = (Int, Int)
type Key = Int
type NodeLabel = Key
type Node = (Char, Pos)
type LabGraphNodes = M.Map Pos Char
type NodeData = (NodeLabel, Distance, Key)
type NodeDataMap = M.Map NodeLabel [NodeData]

nullKey :: Key
nullKey = zeroBits

droneKeys :: [Key]
droneKeys = [shift 1 (64-x) | x <- [2..32]]

keyFromChar :: Char -> Key
keyFromChar '@' = droneKeys !! 0
keyFromChar label = shift 1 bitsToShift
    where bitsToShift = ((ord . toLower) label) - (ord 'a')

isKey = isLower
isLock = isUpper

parse :: String -> [String]
parse input = lines input

addPos :: Pos -> Pos -> Pos
addPos (x, y) (x', y') = (x + x', y + y')

subPos :: Pos -> Pos -> Pos
subPos (x, y) (x', y') = (x - x', y - y')

applyGrid lines = map (\(y, row) -> map (\(x, e) -> (e, (x, y))) $ zip [0..] row) $ zip [0..] lines

getObjects lines = sort $ filter ((`notElem` "#.") . fst) $ concat $ applyGrid lines

getGraphNodes :: [String] -> LabGraphNodes
getGraphNodes lines = foldl (\m (l, pos) -> M.insert pos l m) M.empty nodes
    where nodes = sort $ filter ((/='#') . fst) $ concat $ applyGrid lines

getLabelPositions :: [String] -> M.Map Key Pos
getLabelPositions lines = foldl (\m (l, pos) -> M.insert (keyFromChar l) pos m) M.empty nodes
    where nodes = sort $ filter ((/='#') . fst) $ concat $ applyGrid lines

neighbourDeltas = [(0, 1), (0, -1), (1, 0), (-1, 0)]

getNodePrecompData :: Node -> LabGraphNodes -> [NodeData]
getNodePrecompData node nodes = getNodePrecompData' [] (S.singleton (node, 0, nullKey), M.singleton (snd node) True)
    where getNodePrecompData' nData (S.Empty, _) = sort nData
          getNodePrecompData' nData ((curr@((_, pos), _, _) :<| q), visitedMap) =
                getNodePrecompData' (updateData curr nData) $! foldl (processNeighbours curr) (q, visitedMap) $ getNeighbours pos visitedMap
          updateData ((label, _), dist, keys) nData
                | dist == 0 = nData
                | isKey label = (keyFromChar label, dist, keys) : nData
                | otherwise = nData
          processNeighbours ((label, _), dist, keys) (q, visitedMap) pos' = 
                let key = if isLock label then keyFromChar label else nullKey
                    label' = (nodes ! pos')
                in (q |> ((label', pos'), dist + 1, keys .|. key), M.insert pos' True visitedMap)
          getNeighbours pos visitedMap = filter (`M.notMember` visitedMap) $ filter (`M.member` nodes) $ map (addPos pos) neighbourDeltas

getPrecompData :: LabGraphNodes -> NodeDataMap
getPrecompData nodes = foldl updateMap M.empty $ filter (not.isUpper.snd)  $ filter ((/='.').snd) $ M.toList nodes
    where updateMap dataMap (pos, label) = M.insert (keyFromChar label) (getNodePrecompData (label, pos) nodes) dataMap

extractLabelBits labels = foldl (\l i -> if testBit labels i then i:l else l) [] [0..62]

extractLabelChars labels = map mapToChar $ extractLabelBits labels
                where mapToChar i
                        | i >= 32 = '@': show (62-i)
                        | otherwise = [['a'..] !! i]

getReachable :: NodeLabel -> Key -> NodeDataMap -> [NodeData]
getReachable labels withKeys precompData = concat $ map mapLabels $ extractLabelBits labels
    where mapLabels labelBit = map (\(l, d, k) -> (l .|. (labels `clearBit` labelBit), d, k)) $ nodeData $ bit labelBit
          nodeData label = filter (\(l, _, keysNeeded) -> (keysNeeded .&. withKeys) == keysNeeded && (l .|. withKeys) /= withKeys) $ precompData ! label


getMinimalTraverseWithMem precompData startingKeys = fst $ getMinimalTraverseWithMem' startingKeys nullKey (M.empty :: M.Map (NodeLabel, Key) Distance)

    where allKeys = let numKeys = length $ filter (\(k, _) -> k < 2^32) $ M.toList precompData 
                    in 2^numKeys - 1

          foldReachable keys (minDist', mem') (label', dist', _) =
            let (minDist'', mem'') = getMinimalTraverseWithMem' label' (keys .|. label') mem'
            in (min minDist' (minDist'' + dist'), mem'')

          traverseReachables keys memoize reachables = case reachables of
            (x:y:rest) -> foldl (foldReachable keys) (2^63-1, memoize) reachables
            [(label', dist', _)] -> let (minDist, memoize') = getMinimalTraverseWithMem' label' (keys .|. label') memoize
                                    in (minDist + dist', memoize')

          getMinimalTraverseWithMem' labels keys memoize =
            if (keys .&. allKeys) == allKeys
            then (0, memoize)
            else case memoize !? (labels, keys) of
                    Just distance -> (distance, memoize)
                    Nothing -> let reachables = getReachable labels keys precompData
                                   (minDist, memoize') = traverseReachables keys memoize reachables
                               in (minDist, M.insert (labels, keys) minDist memoize')

isQadrant (x', y') 0 = x' < 0 && y' < 0
isQadrant (x', y') 1 = x' < 0 && y' >= 0
isQadrant (x', y') 2 = x' >= 0 && y' < 0
isQadrant (x', y') 3 = x' >= 0 && y' >= 0

partitionMaze precompData labelPos pos@(x, y) = foldl partitionMaze' M.empty $ M.toList precompData
    where partitionMaze' precompData' (key, reachables) = if key == droneKeys !! 0
                                                          then partitionDroneData precompData' reachables
                                                          else M.insert key (filter (sameQuadrant key) reachables) precompData'
          partitionDroneData precompData' reachables' = let parts = map (filterQuadrant reachables') [0..3]
                                                            offsetd = (map.map) (\(l, d, k) -> (l, d -2, k)) parts
                                                        in foldl (\d (i, r) -> M.insert (droneKeys !! i) r d) precompData' $ zip [0..] offsetd
          filterQuadrant reachables' i = filter (\(l, _, _) -> let pos' = labelPos ! l in isQadrant (subPos pos pos') i) reachables'
          sameQuadrant key' (key'', _, _) = let (x', y') = labelPos ! key'
                                                (x'', y'') = labelPos ! key''
                                            in signum (x - x') == signum (x - x'') && signum (y - y') == signum (y - y'')
--main :: IO ()
main = do
    input <- fmap parse getContents
    labelPositions <- return $ getLabelPositions input
    precompData <- return $ getPrecompData $ getGraphNodes input
    partitionedData <- return $ partitionMaze precompData labelPositions $ labelPositions ! (droneKeys !! 0)
    print $ getMinimalTraverseWithMem partitionedData $ foldl1 (.|.) $take 4 droneKeys
