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

fnbrs80 (w,(z,(y,x))) = [(w2,(z2,(y2,x2))) | w2 <- [w-1..w+1], z2 <- [z-1..z+1], y2 <- [y-1..y+1], x2 <- [x-1..x+1], (w2,z2,y2,x2) /= (w,z,y,x)]
fnbrs26 (z,(y,x)) = [(z2,(y2,x2)) | z2 <- [z-1..z+1], y2 <- [y-1..y+1], x2 <- [x-1..x+1], (z2,y2,x2) /= (z,y,x)]
fnbrs8 (y,x) = [(y2,x2) | y2 <- [y-1..y+1], x2 <- [x-1..x+1], (y2,x2) /= (y,x)]
fnbrs4 (y,x) = [(y-1,x), (y+1,x), (y,x-1), (y,x+1)]

parse = map (=='#')

coordify input = M.fromList $ concat $ map repair $ zip [0..] $ map (zip [0..]) input
    where repair (y,xs) = map (\(x,c) -> ((0,(y,x)),c)) xs

coordify4 input = M.fromList $ concat $ map repair $ zip [0..] $ map (zip [0..]) input
    where repair (y,xs) = map (\(x,c) -> ((0,(0,(y,x))),c)) xs

type Coord = (Int, (Int, Int))

step state = M.fromList [ ((z2,(y2,x2)), cube (z2,(y2,x2))) | z2 <- [minz-1..maxz+1], y2 <- [miny-1..maxy+1], x2 <- [minx-1..maxx+1] ]
    where (zs,yxs) = unzip $ M.keys state
          (ys,xs) = unzip yxs
          (minz,maxz) = (minimum zs, maximum zs)
          (minx,maxx) = (minimum xs, maximum xs)
          (miny,maxy) = (minimum ys, maximum ys)
          cube :: Coord -> Bool
          cube (z2,(y2,x2)) =
              case (oldstate, num) of
                  (True,2) -> True
                  (True,3) -> True
                  (True,_) -> False
                  (False,3) -> True
                  (False,_) -> False
              where oldstate = maybe False id $ M.lookup (z2,(y2,x2)) state
                    nbrs = fnbrs26 (z2,(y2,x2))
                    num = length $ filter id $ map (\nbr -> maybe False id $ flip M.lookup state nbr) nbrs


step4 state = M.fromList [ ((w2,(z2,(y2,x2))), cube (w2,(z2,(y2,x2)))) | w2 <- [minw-1..maxw+1], z2 <- [minz-1..maxz+1], y2 <- [miny-1..maxy+1], x2 <- [minx-1..maxx+1] ]
    where (ws,zyxs) = unzip $ M.keys state
          (zs,yxs) = unzip zyxs
          (ys,xs) = unzip yxs
          (minw,maxw) = (minimum ws, maximum ws)
          (minz,maxz) = (minimum zs, maximum zs)
          (minx,maxx) = (minimum xs, maximum xs)
          (miny,maxy) = (minimum ys, maximum ys)
          cube (w2,(z2,(y2,x2))) =
              case (oldstate, num) of
                  (True,2) -> True
                  (True,3) -> True
                  (True,_) -> False
                  (False,3) -> True
                  (False,_) -> False
              where oldstate = maybe False id $ M.lookup (w2,(z2,(y2,x2))) state
                    nbrs = fnbrs80 (w2,(z2,(y2,x2)))
                    num = length $ filter id $ map (\nbr -> maybe False id $ flip M.lookup state nbr) nbrs

solve f state = length $ filter id $ M.elems $ iterate f state !! 6

main = do input <- map parse <$> lines <$> readFile "input.txt"
          print $ solve step $ coordify input
          print $ solve step4 $ coordify4 input
