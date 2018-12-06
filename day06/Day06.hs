{-# LANGUAGE FlexibleContexts, TupleSections, MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord
import Control.Monad.State
import Control.Arrow
import Debug.Trace

distance (x0,y0) (x1,y1) = abs (x0-x1) + abs (y1-y0)

safe input (x,y) =
    let distances = map (\(coord,id) -> (id,distance coord (x,y))) $ zip input [0..]
    in if (sum (map snd distances) < 10000) then 1 else 0

closestnoob input (x,y) =
    let distances = map (\(coord,id) -> (id,distance coord (x,y))) $ zip input [0..]
        closestiddist = minimumBy (comparing snd) $ map (\(coord,id) -> (id,distance coord (x,y))) $ zip input [0..]
        tie = any (\(id,coord) -> id /= fst closestiddist && coord == snd closestiddist) distances
    in if tie then -1 else fst closestiddist

part1 input =
    let maxx = maximum $ map fst input
        maxy = maximum $ map snd input
        grid = [ (x,y) | x <- [0..maxx], y <- [0..maxy] ]
        voro :: [((Int, Int), Int)]
        voro = map (\coord -> (coord, closestnoob input coord)) grid
        edge1 = filter ((== 0) . fst . fst) voro
        edge2 = filter ((== maxx) . fst . fst) voro
        edge3 = filter ((== 0) . snd .fst) voro
        edge4 = filter ((== maxy) . snd .fst) voro
        infis = nub $ map snd $ edge1 ++ edge2 ++ edge3 ++ edge4
        scores = map (\id -> (id, length $ filter ((==id).snd) voro)) [0..length input-1]
    in last $ sort $ map snd $ filter (\(id,score) -> not $ elem id infis) scores

part2 input =
    let maxx = maximum $ map fst input
        maxy = maximum $ map snd input
        grid = [ (x,y) | x <- [0..maxx], y <- [0..maxy] ]
        safes = map (safe input) grid
    in sum safes

main = print (part1 input, part2 input)

testinput = [
    (1, 1),
    (1, 6),
    (8, 3),
    (3, 4),
    (5, 5),
    (8, 9)]

input = [
    (336, 308),
    (262, 98),
    (352, 115),
    (225, 205),
    (292, 185),
    (166, 271),
    (251, 67),
    (266, 274),
    (326, 85),
    (191, 256),
    (62, 171),
    (333, 123),
    (160, 131),
    (211, 214),
    (287, 333),
    (231, 288),
    (237, 183),
    (211, 272),
    (116, 153),
    (336, 70),
    (291, 117),
    (156, 105),
    (261, 119),
    (216, 171),
    (59, 343),
    (50, 180),
    (251, 268),
    (169, 258),
    (75, 136),
    (305, 102),
    (154, 327),
    (187, 297),
    (270, 225),
    (190, 185),
    (339, 264),
    (103, 301),
    (90, 92),
    (164, 144),
    (108, 140),
    (189, 211),
    (125, 157),
    (77, 226),
    (177, 168),
    (46, 188),
    (216, 244),
    (346, 348),
    (272, 90),
    (140, 176),
    (109, 324),
    (128, 132)]
