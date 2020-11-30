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
type NodeLabel = Char
type Node = (NodeLabel, Pos)
type LabGraphNodes = M.Map Pos NodeLabel
type Key = Int
type NodeData = (NodeLabel, Distance, Key)
type NodeDataMap = M.Map NodeLabel [NodeData]

keyFromLabel :: NodeLabel -> Key
keyFromLabel label = shift 1 bitsToShift
    where bitsToShift = ((ord . toLower) label) - (ord 'a')

nullKey :: Key
nullKey = zeroBits

isKey = isLower
isLock = isUpper

parse :: String -> [String]
parse input = lines input

addPos :: Pos -> Pos -> Pos
addPos (x, y) (x', y') = (x + x', y + y')

applyGrid lines = map (\(y, row) -> map (\(x, e) -> (e, (x, y))) $ zip [0..] row) $ zip [0..] lines

getObjects lines = sort $ filter ((`notElem` "#.") . fst) $ concat $ applyGrid lines

getGraphNodes :: [String] -> LabGraphNodes
getGraphNodes lines = foldl (\m (l, pos) -> M.insert pos l m) M.empty nodes
    where nodes = sort $ filter ((/='#') . fst) $ concat $ applyGrid lines

neighbourDeltas = [(0, 1), (0, -1), (1, 0), (-1, 0)]

getNodePrecompData :: Node -> LabGraphNodes -> [NodeData]
getNodePrecompData node nodes = getNodePrecompData' [] (S.singleton (node, 0, nullKey), M.singleton (snd node) True)
    where getNodePrecompData' nData (S.Empty, _) = sort nData
          getNodePrecompData' nData ((curr@((_, pos), _, _) :<| q), visitedMap) =
                getNodePrecompData' (updateData curr nData) $! foldl (processNeighbours curr) (q, visitedMap) $ getNeighbours pos visitedMap
          updateData ((label, _), dist, keys) nData
                | dist == 0 = nData
                | isKey label = (label, dist, keys) : nData
                | otherwise = nData
          processNeighbours ((label, _), dist, keys) (q, visitedMap) pos' = 
                let key = if isLock label then keyFromLabel label else nullKey
                    label' = (nodes ! pos')
                in (q |> ((label', pos'), dist + 1, keys .|. key), M.insert pos' True visitedMap)
          getNeighbours pos visitedMap = filter (`M.notMember` visitedMap) $ filter (`M.member` nodes) $ map (addPos pos) neighbourDeltas

getPrecompData :: LabGraphNodes -> NodeDataMap
getPrecompData nodes = foldl updateMap M.empty $ filter (not.isUpper.snd)  $ filter ((/='.').snd) $ M.toList nodes
    where updateMap dataMap (pos, label) = M.insert label (getNodePrecompData (label, pos) nodes) dataMap

getReachable :: NodeLabel -> Key -> NodeDataMap -> [NodeData]
getReachable fromLabel withKeys precompData = filter (\(_, _, keysNeeded) -> (keysNeeded .&. withKeys) == keysNeeded) nodeData
    where nodeData = precompData ! fromLabel

-- getMinimalTraverse precompData = getMinimalTraverse' ([], 0, nullKey) M.empty '@'
-- where numKeys = M.size precompData - 1
--       getMinimalTraverse' (path, dist, keys) visitedMap fromLabel
--             | length path == numKeys = (dist, reverse path)
--             | otherwise = let mapReachable (l, d, _) = getMinimalTraverse' (l:path, dist + d, keys .|. (keyFromLabel l)) (M.insert l True visitedMap) l
--                           in minimum $ map mapReachable $ filter (\(l, _, _) -> l `M.notMember` visitedMap) $ getReachable fromLabel keys precompData


getMinimalTraverse precompData = getMinimalTraverse' '@' 0 nullKey
    where allKeys = 2^(M.size precompData - 1) - 1
          getMinimalTraverse' fromLabel keys dist
                | keys == allKeys = dist
                | otherwise = let mapReachable (l, d, _) = getMinimalTraverse' l (keys .|. (keyFromLabel l)) (dist + d)
                                  minDist = minimum $ map mapReachable $ filter (\(l, _, _) -> ((keyFromLabel l) .&. keys) == nullKey) $ getReachable fromLabel keys precompData
                              in minDist

getMinimalTraverseWithMem precompData = fst $ getMinimalTraverseWithMem' '@' nullKey (M.empty :: M.Map (NodeLabel, Key) Distance)

    where allKeys = 2^(M.size precompData - 1) - 1

          foldReachable keys (minDist', mem') (label', dist', _) =
            let (minDist'', mem'') = getMinimalTraverseWithMem' label' (keys .|. (keyFromLabel label')) mem'
            in (min minDist' (minDist'' + dist'), mem'')

          traverseReachables keys memoize reachables = case reachables of
            (x:y:rest) -> foldl (foldReachable keys) (2^63-1, memoize) reachables
            [(label', dist', _)] -> let (minDist, memoize') = getMinimalTraverseWithMem' label' (keys .|. (keyFromLabel label')) memoize
                                    in (minDist + dist', memoize')

          getMinimalTraverseWithMem' fromLabel keys memoize =
            if keys == allKeys
            then (0, memoize)
            else case memoize !? (fromLabel, keys) of
                    Just distance -> (distance, memoize)
                    Nothing -> let reachables = filter (\(l, _, _) -> ((keyFromLabel l) .&. keys) == nullKey) $ getReachable fromLabel keys precompData
                                   (minDist, memoize') = traverseReachables keys memoize reachables
                               in (minDist, M.insert (fromLabel, keys) minDist memoize')


--main :: IO ()
main = do
    input <- fmap parse getContents
    precompData <- return $ getPrecompData $ getGraphNodes input
    print $ getMinimalTraverseWithMem precompData
