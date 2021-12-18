{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

data Square = Wall | Open | Number Int deriving (Eq, Ord, Show)

data SearchState = S { visited :: [(Int, Int)], eaten :: [Int], path :: [(Int,Int)] } deriving (Eq, Ord, Show)

-- eat n input = map (map (\x -> if x == Number n then Open else x)) input

visit loc (S vs es ps) = S (loc:vs) es (loc:ps)
eat n (S vs es ps) | any (== n) es = S vs es ps
                   | otherwise     = S [] (n:es) ps

parse '#' = Wall
parse '.' = Open
parse x = Number $ read [x]

nbr :: [[Square]] -> SearchState -> (Int, Int) -> Maybe ((Int, Int), SearchState)
nbr input state (x,y) =
    if any (==(x,y)) $ visited state then Nothing
    else let nbr' (Wall) = Nothing
             nbr' (Open) = Just $ ((x,y), visit (x,y) state)
             nbr' (Number n) = Just $ ((x,y), visit (x,y) $ eat n state)
         in nbr' $ input !! y !! x

traceSelf x = traceShow x x
nbrs input ((x,y),state) = prune $ catMaybes $ map (nbr input state) [(x-1,y), (x+1,y), (x,y+1), (x,y-1)]

-- prune input = let maxeaten = maximum $ map (length . eaten . snd) input
--               in filter ((== maxeaten) . length . eaten . snd) input
prune = id

goal = (== 5) . length . eaten . snd

solve input =
    let init_y = fromJust $ findIndex (any (== Number 0)) input
        init_x = fromJust $ findIndex (== Number 0) $ input !! init_y
        init_state = S [(init_x, init_y)] [0] []
        bfs :: [[((Int, Int), SearchState)]]
        bfs = iterate (traceSelf . (concatMap $ nbrs input)) [((init_x,init_y),init_state)]
    -- in takeWhile (not . goal) bfs
    in path $ snd $ fromJust $ find goal $ until (any goal) (traceSelf . (concatMap $ nbrs input)) [((init_x,init_y),init_state)]

main = interact $ (++"\n") . show . solve . map (map parse) . lines
