{-# LANGUAGE FlexibleContexts, TupleSections, MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Bits
import Data.Maybe
import Data.Char
import Data.Ord
import Control.Monad.State
import Control.Arrow
import Debug.Trace

type Coord = (Int, Int)
type Map = S.Set Coord 

spring = (0,500)

parse :: Map -> [String] -> Map
parse m ["x",x,"y",y0,y1] = S.union m $ S.fromList $ map (\y -> (y, read x)) [((read y0) :: Int)..((read y1) :: Int)]
parse m ["y",y,"x",x0,x1] = S.union m $ S.fromList $ map (\x -> (read y, x)) [((read x0) :: Int)..((read x1) :: Int)]

findDrySandLeft clays water (y,x) | S.member (y,x) clays = False
findDrySandLeft clays water (y,x) | S.member (y,x) water = findDrySandLeft clays water (y,x-1)
findDrySandLeft clays water (y,x) = True

findDrySandRight clays water (y,x) | S.member (y,x) clays = False
findDrySandRight clays water (y,x) | S.member (y,x) water = findDrySandRight clays water (y,x+1)
findDrySandRight clays water (y,x) = True

getnextwater clays water (y,x) =
    -- not (isDrySand clays water (y+1,x-1) || isDrySand clays water (y+1,x+1)) && -- diag down
    not (findDrySandLeft  clays water (y+1,x-1) ||
         findDrySandRight clays water (y+1,x+1)) &&
    (isDrySand clays water (y,x-1) || isDrySand clays water (y,x+1))

bfs clays water [] =
    let nextwater = S.filter (getnextwater clays water) water
    in if S.null nextwater then water -- traceShow (output clays water) water
       else -- traceShow (output clays water) $ traceShow nextwater $
                bfs clays water $ S.toList nextwater
    -- if undefined -- TODO: search for next falling water up in the path to be frontier
    -- then
    --     undefined -- TODO add it to frontier (ok to stay in water)
    -- else S.toList water
bfs clays water ((w@(y,x)):frontier) = -- traceShow (y,x) $
    if isDrySand clays water (y+1,x) then
        bfs clays (S.insert (y+1,x) water) ((y+1,x):frontier) -- sand below water, flow into it
    else if y == maximum (map fst $ S.toList clays) then bfs clays water frontier else
        let (w1,f1) = if isDrySand clays water (y,x-1) then -- flow left?
                          (S.insert (y,x-1) water, (y,x-1):frontier) -- FIXME: dfs? bfs?
                      else (water,frontier)
            (w2,f2) = if isDrySand clays water (y,x+1) then -- flow right?
                          (S.insert (y,x+1) w1, (y,x+1):f1) -- FIXME
                      else (w1,f1)
        in bfs clays w2 f2

isSand clays (y,x) = S.notMember (y,x) clays && y <= maximum (map fst $ S.toList clays)
isDrySand clays water (y,x) = isSand clays (y,x) && S.notMember (y,x) water

output :: Map -> Map -> [String]
output clays water =
    let minx = minimum $ map snd $ S.toList clays
        maxx = maximum $ map snd $ S.toList clays
        miny = 0 -- minimum $ map fst $ S.toList clays
        maxy = maximum $ map fst $ S.toList clays
        cell y x = if S.member (y,x) clays then '#' else if S.member (y,x) water then '~' else '.'
        row y = map (cell y) [minx..maxx]
    in map row [miny..maxy]

main = do input <- map words <$> lines <$> readFile "input.txt"
          let clays = foldl parse S.empty input
          -- mapM print $ output clays $ S.fromList [spring]
          let flow = bfs clays S.empty [spring]
          mapM print $ output clays flow
          print $ S.size flow
