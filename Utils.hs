module Utils where

import Data.List
import Data.Maybe
import Control.Monad
import Control.Arrow

import Crypto.Hash.MD5
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base16 (encode)

-- udlr can be "UDLR", "^v<>", etc
-- ex. fnbrs "UDLR" (5,5) 'U' = (5,4)
nbr :: Num a => String -> Char -> (a, a) -> (a, a)
nbr udlr c | elem c udlr = ([second, first] <*> [subtract 1, (+1)]) !! (fromJust $ elemIndex c udlr)
           | otherwise   = error "nbrs: c not in UDLR"

fnbrs :: Num a => (a, a) -> [(a, a)]
fnbrs x = map (flip (nbr "UDLR") x) "UDLR"

powerset :: [a] -> [[a]]
powerset = filterM $ const [True, False]

stringhash :: String -> String
stringhash = unpack . encode . hash . pack

shortestpath :: Num b => [((a,a), b)] -> [a]
shortestpath graph = undefined -- TODO

shortestcycle :: Num b => [((a,a), b)] -> [a]
shortestcycle graph = undefined -- TODO

takeAllBut :: Int -> [a] -> [a]
takeAllBut n = reverse . drop n . reverse

toPath :: [a] -> [(a,a)]
toPath l = zip l $ tail l

toCycle :: [a] -> [(a,a)]
toCycle l = zip l $ tail $ cycle l

partitions :: Int -> Int -> [[Int]]
partitions 0       gold = undefined
partitions pirates 0    = [replicate pirates 0]
partitions 1       gold = [[gold]]
partitions pirates gold = concatMap (\g -> map (g:) $ partitions (pirates-1) (gold-g)) [0..gold]

chineseRemainder :: Integral a => (a, a) -> (a, a) -> (a, a)
chineseRemainder (base,incr) (prime, remainder) =
    (fromJust $ find ((== remainder) . (`mod` prime)) [base, base+incr..], incr * prime)

knapsack :: Int -> [Int] -> [[Int]]
knapsack limit xs | limit < 0 || sum xs < limit = []
                  | limit == 0 = [[]]
                  | sum xs == limit = [xs]
knapsack limit (x:xs) = map (x:) (knapsack (limit-x) xs) ++ knapsack limit xs

index :: Int -> [a] -> [a] -- returns singleton or empty list, like !! but supports failure
index n l = take 1 $ drop n l

diagonalize :: [[a]] -> [[a]] -- works on infinite lists
diagonalize g = takeWhile (not . null) $ map diag [0..]
    where diag n = concatMap (element n) [0..n]
          element n m = [ x | y <- index (n-m) g, x <- index m y ]

-- finds all "thread interleavings"
-- e.g. sched ["aa","bb"] = ["aabb", "abab", "abba", "baab", "baba", "bbaa"]
sched :: Eq a => [[a]] -> [[a]]
sched ts | all null ts = [[]]
sched ts = [ head t : s | t <- filter (not . null) ts, s <- sched $ tail t : delete t ts ]

