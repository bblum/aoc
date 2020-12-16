{-# LANGUAGE FlexibleContexts, TupleSections, MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.List.Split
import Data.Either
import Data.Maybe
import Data.Char
import Data.Ord
import Control.Monad.State
import Control.Arrow
import Debug.Trace

-- fromList [(0,17),(1,5),(2,15),(3,6),(4,4),(5,18),(6,2),(7,16),(8,1),(9,7),(10,3),(11,13),(12,12),(13,10),(14,11),(15,19),(16,8),(17,0),(18,14),(19,9)]


me = [139,67,71,59,149,89,101,83,107,103,79,157,151,113,61,109,73,97,137,53]

-- TODO name the rules
parse :: [String] -> [Int]
parse x = parse' $ dropWhile (/= ":") x
parse' [":",a,b,"or",c,d] = map read [a,b,c,d]

valid num [a,b,c,d] = f a b || f c d where f x y = num >= x && num <= y

invalid :: [[Int]] -> Int -> Bool
invalid rules num = all (not . valid num) rules

match :: [[Int]] -> Int -> [Int] -- returns ruleidx
match rules num = map fst $ filter (valid num . snd) $ zip [0..] rules

type SeenMap = M.Map Int Int -- rule idx -> ticket idx

allmatch :: [[Int]] -> [Int] -> [Int] -- returns matching ruleidxs
allmatch rules vals = map fst $ filter (\(_,rule) -> all (\num -> valid num rule) vals) $ zip [0..] rules

check :: [[Int]] -> SeenMap -> (Int, [Int]) -> SeenMap
check rules seen (ticket_idx,vals) = if elem ticket_idx $ M.elems seen then seen else ans
    where ans = ck $ filter (not . flip M.member seen) $ allmatch rules vals
          ck [] = error $ "impossible " ++ show (ticket_idx,vals)
          ck [ruleidx] = traceShow ("fixed rule",ruleidx,rules!!ruleidx,"->",ticket_idx) $ ans
            where ans = M.insert ruleidx ticket_idx seen
          ck xs = traceShow ("ambiguous",xs,"?? ->",ticket_idx) seen -- multiple matching rules, no progress

-- check_ticket rules seen ticket = foldl (check rules) seen $ zip [0..] ticket

search rules tickets seen = if M.size seen == length rules then seen else if newseen == seen then error "converged too soon" else search rules tickets newseen
    where newseen = foldl (check rules) seen $ zip [0..] $ transpose tickets


main = do rules <- map (parse . words) <$> lines <$> readFile "input.txt"
          nearby <- map (map read . words) <$> lines <$> readFile "nearby.txt"
          print $ sum $ filter (invalid rules) $ concat nearby
          let valids = filter (\ticket -> not $ any (invalid rules) ticket) nearby
          let key = search rules valids M.empty
          print key
          let idxs = take 6 $ M.elems key
          print $ product $ map (me !!) idxs
