import Data.List (transpose)
import qualified Data.Sequence as S
import qualified Data.Map.Strict as M
import Data.Map ((!), (!?))
import Data.Sequence ((<|), (|>), Seq((:<|)), Seq((:|>)))

type SubstSymbol = String
type SubstQuant = Integer
type Reactions = M.Map SubstSymbol (SubstQuant, [(SubstQuant, SubstSymbol)])
type StoichiometryMap = M.Map SubstSymbol (SubstQuant, Int)
type ReactionQueue = S.Seq String

parse :: String -> Reactions
parse input = M.fromList sides'
   where sides = map (\line -> words [if c == ' ' then ';' else (if c == '=' then ' ' else c) | c <- line, c /= '>']) $ lines input
         sides' = map (\[ls, rs] -> let (pls, prs) = (parseLeft ls, parseRight rs) in (prs !! 1, (read (prs !! 0), zip (map read (pls !! 0)) (pls !! 1)))) sides
         parseLeft side =  transpose $ map parseRight $ words [if c == ',' then ' ' else c | c <- side]
         parseRight side = words [if c == ';' then ' ' else c | c <- side]

initMap :: Reactions -> StoichiometryMap
initMap reactions = foldl processRightSide M.empty reactions
   where processRightSide stMap (_, rightSide) = foldl processSymbol stMap rightSide
         processSymbol stMap (_, symbol) = M.insertWith (\(_, n) (_, o) -> (0, n + o)) symbol (0, 1) stMap

addQuantity :: SubstSymbol -> SubstQuant -> StoichiometryMap -> StoichiometryMap
addQuantity symbol quant stMap = M.insertWith (\(n, _) (o, c) -> (n + o, c - 1)) symbol (quant, 0) stMap

getQuantity :: SubstSymbol -> StoichiometryMap -> SubstQuant
getQuantity symbol stMap = fst $ M.findWithDefault (0, 0) symbol stMap

ceilQuantity :: SubstSymbol -> SubstQuant -> Reactions -> SubstQuant
ceilQuantity symbol quant reactions = quant `div` leftSideQuant + signum (quant `mod` leftSideQuant)
   where (leftSideQuant, _) = reactions ! symbol

calculateQuantities :: SubstQuant -> SubstSymbol -> Reactions -> StoichiometryMap
calculateQuantities rootQuant rootSymbol reactions  = calculateQuantities' (S.singleton rootSymbol, initialStMap)
   where initialStMap = addQuantity rootSymbol rootQuant $ initMap reactions
         calculateQuantities' (S.Empty, stMap) = stMap
         calculateQuantities' ((symbol :<| q), stMap) = calculateQuantities' $ updateQuantities symbol q stMap
         updateQuantities symbol q stMap =
            let reaction@(_, rightSide) = reactions ! symbol
                leftSideQuant = ceilQuantity symbol (fst $ stMap ! symbol) reactions
            in foldl (updateQuantity leftSideQuant) (q, stMap) rightSide
         updateQuantity leftSideQuant (q, stMap) (quant, symbol) =
            let stMap' = addQuantity symbol (leftSideQuant * quant) stMap
                doEnqueue = (snd $ stMap' ! symbol) == 0 && (symbol `M.member` reactions)
            in if doEnqueue
               then (q |> symbol, stMap')
               else (q, stMap')

producedQuantity :: SubstSymbol -> SubstQuant -> SubstSymbol -> Reactions -> SubstQuant
producedQuantity produceSymbol substrateQuant substrateSymbol reactions = producedQuantity' (estimation, 2 * estimation)
   where estimation = substrateQuant `div` (getQuantity substrateSymbol $ calculateQuantities 1 produceSymbol reactions)
         producedQuantity' (lower, upper) =
            let midpoint = (upper + lower) `div` 2
                quant = getQuantity substrateSymbol $ calculateQuantities midpoint produceSymbol reactions
            in if quant == substrateQuant
               then midpoint
               else if lower == upper
                    then if quant <= substrateQuant then lower else lower - 1
                    else if quant < substrateQuant
                         then producedQuantity' (midpoint + 1, upper)
                         else producedQuantity' (lower, midpoint)

main = do
   reactions <- fmap parse getContents
   print $ producedQuantity "FUEL" 1000000000000 "ORE" reactions