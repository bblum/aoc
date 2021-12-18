{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad
import Control.Arrow
import Debug.Trace

type Coord = (Int, Int)

nbrs :: [String] -> (Coord, [Coord]) -> [(Coord, [Coord])]
nbrs input ((x,y), path) =
    let visit (p@(x,y)) = guard (input !! y !! x /= '#' && not (elem p path)) >> Just (p, p:path)
    in catMaybes $ map visit [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

eatOne :: [String] -> ((Coord, [Coord]), [Coord]) -> ((Coord, [Coord]), [Coord])
eatOne input ((p0, path0), eaten) =
    let goal ((x,y),_) = isDigit (input !! y !! x) && not (elem (x,y) eaten)
        foo x = any goal x || x == []
        Just (p, path) = find goal $ until foo (concatMap $ nbrs input) [(p0, [])]
    in ((p, path ++ path0), p:eaten)

eatAll input =
    let Just y0 = findIndex (any (== '0')) input
        Just x0 = findIndex (== '0') $ input !! y0
        n = length $ concat $ map (filter isDigit) input
    in until ((== n) . length . snd) (join traceShow . eatOne input) (((x0,y0), []), [(x0,y0)])

main = interact $ (++"\n") . show . length . snd . fst . eatAll . lines
