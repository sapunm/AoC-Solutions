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
type PortalMap = M.Map Pos Pos

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
    where portalMap = foldl addPortalPair M.empty paired
          addPortalPair m [(_, a', b'), (_, a'', b'')] = M.insert b' a'' $ M.insert b'' a' m
          paired = groupBy (\(n, _, _) (n', _, _) -> n == n') $ tail $ init portalList
          entrance = let (_, pos, _) = head portalList in pos
          exit = let (_, pos, _) = last portalList in pos
          portalList = sort $ map mapPosition $ concat [rows, columns]
          mapPosition [(a@(x, y), l), (b@(x', y'), l')]
                | x == x' = let a' = (x, y - 1)
                                b' = (x, y' + 1)
                                name = [l, l']
                            in if a' `M.member` nodes then (name, a', a) else (name, b', b)
                | y == y' = let a' = (x - 1, y)
                                b' = (x' + 1, y)
                                name = [l, l']
                            in if a' `M.member` nodes then (name, a', a) else (name, b', b)
          columns = concat $ filter (/=[]) $ map (nonEmpty . groups) $ transpose coordinates
          rows = concat $ filter (/=[]) $ map (nonEmpty . groups) coordinates
          nonEmpty groups = filter ((>1) . length) $ filter (isUpper.snd.head) groups
          groups line = groupBy (\(_, c) (_, c') -> isUpper c && isUpper c') line
          coordinates = applyCoordinates lines

getShortestPath :: Pos -> Pos -> NodeMap -> PortalMap -> Maybe Distance
getShortestPath entrance exit nodes portalMap = getShortestPath' (S.singleton (entrance, 0), M.singleton (entrance) True)

    where getShortestPath' (S.Empty, _) = Nothing

          getShortestPath' (curr@(pos, dist) :<| q, visitedMap) =
                if pos == exit
                then Just dist
                else getShortestPath' $! foldl (processNeighbours dist) (q, visitedMap) $ getNeighbours pos visitedMap

          processNeighbours dist (q, visitedMap) pos' = (q |> (pos', dist + 1), M.insert pos' True visitedMap)

          travelPortals pos = case portalMap !? pos of
                Just pos' -> pos'
                Nothing -> pos

          getNeighbours pos visitedMap = filter (`M.notMember` visitedMap) $ filter (`M.member` nodes) $ map travelPortals $ map (addPos pos) neighbourDeltas

main = do
    input <- fmap parse getContents
    nodes <- return $ getNodes input
    (entrance, exit, portals) <- return $ getPortals input nodes
    print $ getShortestPath entrance exit nodes portals
