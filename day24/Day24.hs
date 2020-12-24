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

parse [] = []
parse [x] = [[x]]
parse ('n':'e':rest) = "ne":parse rest
parse ('n':'w':rest) = "nw":parse rest
parse ('s':'e':rest) = "se":parse rest
parse ('s':'w':rest) = "sw":parse rest
parse (x:rest) = [x]:parse rest

fnbrs6 (y,x) = [(y,x-2),(y,x+2),(y-1,x-1),(y-1,x+1),(y+1,x-1),(y+1,x+1)]

coord yx [] = yx
coord (y,x) ("n":rest) = error "hex"
coord (y,x) ("e":rest) = coord (y,x+2) rest
coord (y,x) ("s":rest) = error "hex"
coord (y,x) ("w":rest) = coord (y,x-2) rest
coord (y,x) ("ne":rest) = coord (y-1,x+1) rest
coord (y,x) ("nw":rest) = coord (y-1,x-1) rest
coord (y,x) ("se":rest) = coord (y+1,x+1) rest
coord (y,x) ("sw":rest) = coord (y+1,x-1) rest

lookupdef m yx = case M.lookup yx m of Just x -> x; Nothing -> False

count = length . filter id

insertify m yx = M.insert yx (not $ lookupdef m yx) m

step m yx = ans
    where live_nbrs = count $ map (lookupdef m) $ fnbrs6 yx
          self = lookupdef m yx
          ans = case (self, live_nbrs) of
                    (True, n) | n == 0 || n > 2 -> False
                    (False, n) | n == 2 -> True
                    _ -> self

stepall :: Noob -> Noob
stepall m = M.fromList [((y,x), step m (y,x)) | y <- [miny-1..maxy+1], x <- [minx-2..maxx+2]]
    where (ys,xs) = unzip $ M.keys m
          (maxy,miny) = (maximum ys, minimum ys)
          (maxx,minx) = (maximum xs, minimum xs)

main = do input <- map parse <$> lines <$> readFile "input.txt"
          let start = foldl insertify M.empty $ map (coord (0,0)) input
          print $ count $ M.elems $ start
          print $ count $ M.elems $ iterate stepall start !! 100
