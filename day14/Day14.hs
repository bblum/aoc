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

fnbrs8 (y,x) = [(y2,x2) | y2 <- [y-1..y+1], x2 <- [x-1..x+1], (y2,x2) /= (y,x)]
fnbrs4 (y,x) = [(y-1,x), (y+1,x), (y,x-1), (y,x+1)]

parse = f . words where f [a,b] = (a,b)

step :: [(String, String)] -> String -> String
step rules polymer = (concat $ zipWith insert polymer $ tail polymer) ++ [last polymer]
    where insert a b = a:(fromJust $ lookup [a,b] rules)

part1 polymer = maximum quantities - minimum quantities
    where quantities = map length $ group $ sort polymer

-- part 2:
-- memoized it with a map of ([a,b],depth) -> (map of frequencies)
-- append the maps by adding them at each level

type FrequencyMap = M.Map Char Int
type DepthMap = M.Map ((Char,Char),Int) FrequencyMap

addMaps = foldl $ M.unionWith (+)

generate rules m = M.fromList $ map recurse rules
    where recurse ([a,b],[c]) = ((a,b), addMaps (M.singleton c 1) [m M.! (a,c), m M.! (c,b)])

-- TODO golf this to use 'map $ const M.empty' lmao
-- means changing key type back to list
start ([a,b],_) = ((a,b), M.empty)

solve ans = maximum ts - minimum ts where ts = map snd $ totals ans
totals ans = M.toList $ addMaps templateMap $ map (ans M.!) $ zip template $ tail template
    where templateMap = M.fromList $ map (head &&& length) $ group $ sort template

part n input = iterate (generate input) (M.fromList $ map start input) !! n

main = do input <- map parse <$> lines <$> readFile "input.txt"
          print $ part1 $ iterate (step input) template !! 10
          -- let ans = iterate (generate input) (M.fromList $ map start input) !! 10
          -- mapM print $ M.toList ans
          -- print $ solve ans
          print template
          print $ step input template
          print $ step input $ step input template
          -- mapM print $ M.toList $ part 1 input
          print $ totals $ part 1 input
          print $ solve $ part 40 input

-- template = "NNCB"
template = "PHOSBSKBBBFSPPPCCCHN"

