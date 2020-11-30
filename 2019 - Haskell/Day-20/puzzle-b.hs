import Data.Char (ord, chr, isUpper)
import Data.List (groupBy, transpose, nub, sort, stripPrefix, intersperse, intercalate)
import Data.Maybe (fromJust, isJust, isNothing)
import Data.Either (fromLeft, fromRight)
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M
import Data.Map ((!), (!?))
import Data.Sequence ((<|), (|>), Seq((:<|)), Seq((:|>)))
import Debug.Trace

parse :: String -> [String]
parse input = lines input

type Pos = (Int, Int)
type Distance = Int
type NodeMap = M.Map Pos Bool
type PortalMap = M.Map Pos String
data PortalType = Inner String | Outer String deriving (Eq, Ord, Show)

neighbourDeltas :: [Pos]
neighbourDeltas = [(0, 1), (0, -1), (1, 0), (-1, 0)]

addPos :: Pos -> Pos -> Pos
addPos (x, y) (x', y') = (x + x', y + y')

subPos :: Pos -> Pos -> Pos
subPos (x, y) (x', y') = (x - x', y - y')

dist :: Pos -> Pos -> Distance
dist (x, y) (x', y') = abs(x' - x) + abs(y' - y)

applyCoordinates lines = map (\(y, row) -> map (\(x, e) -> ((x, y), e)) $ zip [0..] row) $ zip [0..] lines

getNodes :: [String] -> NodeMap
getNodes lines = foldl (updateMap) M.empty $ coordinates
   where coordinates = filter ((=='.') . snd) $ concat $ applyCoordinates lines
         updateMap m (pos, _) = M.insert pos True m

--getPortals :: [Int] -> VisitedMap -> ScaffGraph
getPortals lines nodes = (entrance, exit, portalMap)
    where portalMap = M.insert entrance ("AA",-1) $ M.insert exit ("ZZ", -1) $ foldl addPortalPair M.empty paired
          addPortalPair m [(n, a', t'), (_, a'', t'')] = M.insert a' (n, t') $ M.insert a'' (n, t'') m
          paired = groupBy (\(n, _, _) (n', _, _) -> n == n') $ tail $ init portalList
          entrance = let (_, pos, _) = head portalList in pos
          exit = let (_, pos, _) = last portalList in pos
          portalList = sort $ map mapPosition $ concat [rows, columns]
          mapPosition [(a@(x, y), l), (b@(x', y'), l')]
                | x == x' = let a' = (x, y - 1)
                                b' = (x, y' + 1)
                                name = [l, l']
                                type' = signum $ (snd mid) - y
                            in if a' `M.member` nodes then (name, a', type') else (name, b', -1 * type')
                | y == y' = let a' = (x - 1, y)
                                b' = (x' + 1, y)
                                name = [l, l']
                                type' = signum $ (fst mid) - x
                            in if a' `M.member` nodes then (name, a', type') else (name, b', -1 * type')
          columns = concat $ filter (/=[]) $ map (nonEmpty . groups) $ transpose coordinates
          rows = concat $ filter (/=[]) $ map (nonEmpty . groups) coordinates
          nonEmpty groups = filter ((>1) . length) $ filter (isUpper.snd.head) groups
          groups line = groupBy (\(_, c) (_, c') -> isUpper c && isUpper c') line
          mid = (fst size `div` 2, snd size `div` 2)
          size = foldl1 (\((x,y), _) ((x',y'), _) -> (max x (x'+1), max y (y'+1))) $ concat coordinates
          coordinates = applyCoordinates lines


--getNodePrecompData :: Node -> LabGraphNodes -> [NodeData]
getPortalPrecompData portal portals nodes = getPortalPrecompData' [] (S.singleton (snd portal, 0), M.singleton (snd portal) True)
    where getPortalPrecompData' nData (S.Empty, _) = sort nData
          getPortalPrecompData' nData ((curr@(pos, dist) :<| q), visitedMap) =
                getPortalPrecompData' (updateData curr nData) $! foldl (processNeighbours curr) (q, visitedMap) $ getNeighbours pos visitedMap
          updateData (pos, dist) nData
                | dist == 0 = nData
                | pos `M.member` portals = let (name, type') = portals ! pos
                                           in (if type' == -1 then Inner name else Outer name, dist):nData
                | otherwise = nData
          processNeighbours (_, dist) (q, visitedMap) pos' = (q |> (pos', dist + 1), M.insert pos' True visitedMap)
          getNeighbours pos visitedMap = filter (`M.notMember` visitedMap) $ filter (`M.member` nodes) $ map (addPos pos) neighbourDeltas

--getPrecompData :: LabGraphNodes -> NodeDataMap
getPrecompData portals nodes = foldl updateMap M.empty $ M.toList portals
    where updateMap dataMap (pos, label) = M.insert label (getPortalPrecompData (label, pos) portals nodes) dataMap


getMinimalTraverseWithMem precompData fromPortal toPortal = fst $ getMinimalTraverseWithMem' fromPortal 0 M.empty M.empty

   where foldReachable level visitedPortals (minDist', mem') (portal', dualName', dist') =
            let level' = if (take 2 portal', level) `M.member` visitedPortals
                         then level - 1
                         else level + 1
                visited' = M.insert (take 2 portal', level) True visitedPortals
                (minDist'', mem'') = getMinimalTraverseWithMem' dualName' level' visited' mem'
            in (min minDist' (minDist'' + dist'), mem'')
      
         traverseReachables level visitedPortals memoize reachables = case reachables of
                  (x:y:rest) -> foldl (foldReachable level visitedPortals) (2^63-1, memoize) reachables
                  [(portal', dualName', dist')] -> let level' = if (take 2 portal', level) `M.member` visitedPortals 
                                                                then level - 1
                                                                else level + 1
                                                       visited' = M.insert (take 2 portal', level) True visitedPortals
                                                       (minDist, memoize') = getMinimalTraverseWithMem' dualName' level' visited' memoize
                                                   in (minDist + dist', memoize')
                  [] -> (2^63-1, memoize)

         getMinimalTraverseWithMem' portal level visitedPortals memoize =
            if traceShow (portal, level) $portal == toPortal && level == 0
            then traceShow portal $ (0, memoize)
            else let reachables = if level < 16 then filter (\(n, _, _) -> n /= "AA" && (n/="ZZ" || level == 0)) (precompData ! portal) else []
                     (minDist, memoize') = traverseReachables level visitedPortals memoize reachables
                 in (minDist, M.insert (portal, level) minDist memoize')

-- getShortestPath :: Pos -> Pos -> NodeMap -> PortalMap -> Maybe Distance
-- getShortestPath entrance exit nodes portalMap = getShortestPath' (S.singleton (entrance, 0, 0, 0), M.singleton (entrance, 0, 0) True, M.empty)

--     where getShortestPath' (S.Empty, _, _) = Nothing

--           getShortestPath' (curr@(pos, dist, level, dir) :<| q, visitedMap, visitedPortals) =
--                 if pos == exit && level == 0
--                 then Just dist
--                 else traceShow curr $  getShortestPath' $! foldl (processNeighbours curr) (q, visitedMap, visitedPortals) $ getNeighbours pos level dir visitedMap visitedPortals

--           processNeighbours (pos, dist, level, dir) (q, visitedMap, visitedPortals) (pos', level', port', dir') =
--              let q' = q |> (pos', dist + 1, level', dir')
--                  vm' = M.insert (pos', level', dir') True $ M.insert (pos, level, dir * (-1)) True visitedMap
--                  vp' = M.insert (port', level') True visitedPortals
--             in (q', vm', vp')

--           travelPortals level visitedPortals dir pos = case portalMap !? pos of
--                 Just (pos', name) -> case visitedPortals !? (name, level) of
--                                      Just True -> traceShow ("Back " ++ name) $ (pos', max 0 (level - 1), name, -1)
--                                      Nothing -> if dir /= -1
--                                                 then traceShow ("Deep " ++ name) $ (pos', min 32 (level + 1), name, 1)
--                                                 else traceShow ("No Double " ++ name) (pos, level, "", dir)
--                 Nothing -> (pos, level, "", dir)

--           getNeighbours pos level dir visitedMap visitedPortals =
--                 let notVisited = filter (\(p, l, _, d) -> (p, l, d) `M.notMember` visitedMap) inMap
--                     inMap = filter (\(p, _, _, _) -> p `M.member` nodes) withPortals
--                     withPortals = map (travelPortals level visitedPortals dir) plainNeighbours
--                     plainNeighbours = map (addPos pos) neighbourDeltas
--                 in notVisited

main = do
    input <- fmap parse getContents
    nodes <- return $ getNodes input
    (entrance, exit, portals) <- return $ getPortals input nodes
    print entrance
    print exit
    mapM print portals
    --mapM print $ M.toList $ getPrecompData portals nodes 
    --print $ getMinimalTraverseWithMem (M.delete "ZZ" (getPrecompData portals nodes)) "AA" "ZZ"
    -- print $ getShortestPath entrance exit nodes portals
