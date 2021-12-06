import qualified Data.Map as M
import Data.List

me = [139,67,71,59,149,89,101,83,107,103,79,157,151,113,61,109,73,97,137,53]

parse = map read . filter (/= "or") . tail . dropWhile (/= ":")

match x [a,b,c,d] = x >= a && x <= b || x >= c && x <= d

search tickets rules = head $ filter ((== length rules) . M.size) $ iterate scan M.empty
    where scan seen = foldl check seen $ zip [0..] $ transpose tickets
          check seen (column,xs) =
              if elem column $ M.elems seen then seen
              else try $ filter (not . flip M.member seen) $ map fst $ filter rulematches rules
              where rulematches (_,rule) = all (flip match rule) xs
                    try [ruleid] = M.insert ruleid column seen
                    try _ = seen -- ambiguous

main = do rules <- map (parse . words) <$> lines <$> readFile "input.txt"
          nearby <- map (map read . words) <$> lines <$> readFile "nearby.txt"
          let valid num = any (match num) rules
          -- part 1
          print $ sum $ filter (not . valid) $ concat nearby
          -- part 2
          let valids = filter (all valid) nearby
          print $ product $ map (me !!) $ take 6 $ M.elems $ search valids $ zip [0..] rules
