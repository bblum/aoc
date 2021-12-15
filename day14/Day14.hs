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

add = foldl $ M.unionWith (+)

generate rules m = M.fromList $ map recurse rules
    where recurse [[a,b],[c]] = ((a,b), add (M.singleton c 1) [m M.! (a,c), m M.! (c,b)])

-- TODO golf this to use 'map $ const M.empty' lmao
-- means changing key type back to list
start [[a,b],_] = ((a,b), M.empty)

solve ans = maximum totals - minimum totals
    where totals = map snd $ M.toList $ add templateMap $ map (ans M.!) $ zip template $ tail template
          templateMap = M.fromList $ map (head &&& length) $ group $ sort template

part n rules = solve $ iterate (generate rules) (M.fromList $ map start rules) !! n

main = do rules <- map words <$> lines <$> readFile "input.txt"
          print $ part 10 rules
          print $ part 40 rules

-- template = "NNCB"
template = "PHOSBSKBBBFSPPPCCCHN"

