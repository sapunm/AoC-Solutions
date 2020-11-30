import Data.List
import qualified Data.Map.Strict as M

parse :: String -> [[String]]
parse input = map words $ lines input

splitEvery _ [] = []
splitEvery n list = first : (splitEvery n rest)
  where
    (first,rest) = splitAt n list

everyNth n [] = []
everyNth n (x : xs) = x : (everyNth n $ drop (n-1) xs)

incrementIndices deckSize i = take deckSize $ unfoldr (\b -> Just (b, (b + i) `mod` deckSize)) 0

shuffle deck techniquesList = deal deck techniquesList
    where deckSize = length deck
          deal deck' [] = deck'
          deal deck' (tech:rest) = case tech of
            [_, _, "increment", i] -> deal (M.elems $ M.fromList $ zip (incrementIndices deckSize (read i)) deck') rest
            [_, _, "new", "stack"] -> deal (reverse deck') rest
            ["cut", i] -> deal (let (a, b) = splitAt ((deckSize + read i) `mod` deckSize) deck' in b ++ a) rest

main :: IO ()
main = do
    input <- fmap parse getContents
    -- print $ shuffle [0..9] input
    print $ shuffle [0..9] input
    -- print $ elemIndex 2019 $ shuffle [0..10006] input
