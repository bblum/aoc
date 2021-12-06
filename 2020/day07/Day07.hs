import qualified Data.Set as S
import Data.List.Split
import Data.Maybe

parse (a:b:"bags":"contain":"no":_) = ((a, b), [])
parse (a:b:"bags":"contain":rest) = ((a, b), map parsecontents $ splitOn [","] rest)
    where parsecontents [n,a2,b2,_] = ((a2, b2), read n :: Int)

solve1 input goals = if goals == newgoals then S.size goals - 1 else solve1 input newgoals
    where newgoals = S.union goals $ foldl1 S.union $ S.map containers goals
          containers goal = S.fromList $ mapMaybe (contains goal) input
          contains goal (bag, contents) = if elem goal $ map fst contents then Just bag else Nothing

solve2 input bag = sum $ map contains $ fromJust $ lookup bag input
    where contains (newbag, n) = n * (1 + solve2 input newbag)

start = ("shiny", "gold")

main = do input <- map (parse . words) <$> lines <$> readFile "input.txt"
          print $ solve1 input $ S.singleton start
          print $ solve2 input start
