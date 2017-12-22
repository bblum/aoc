{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

solve = undefined

data Facing = L | D | U | R deriving Show

turnleft L = D
turnleft D = R
turnleft U = L
turnleft R = U
turnright L = U
turnright U = R
turnright R = D
turnright D = L

step L (x,y) = (x-1,y)
step R (x,y) = (x+1,y)
step U (x,y) = (x,y-1)
step D (x,y) = (x,y+1)

data Nobe = Clean | Weak | Flag | Inf deriving (Eq, Ord, Show)

updateState Clean = Weak
updateState Weak  = Inf
updateState Flag  = Clean
updateState Inf   = Flag

turn facing Clean = turnleft facing
turn facing Weak  = facing
turn facing Flag  = turnleft $ turnleft facing
turn facing Inf   = turnright facing

burst :: ((Int, Int), Facing, M.Map (Int, Int) Nobe, Int) ->
         ((Int, Int), Facing, M.Map (Int, Int) Nobe, Int) 
burst ((x,y),facing,nobes,infeectioncount) =
    let currentnobe = fromMaybe Clean $ M.lookup (x,y) nobes
        newnobes = M.insert (x,y) (updateState currentnobe) nobes
        newfacing = turn facing currentnobe --if currentnobe then turnright facing else turnleft facing
        newpos = step newfacing (x,y)
        newinfeectioncount = infeectioncount + if updateState currentnobe == Inf then 1 else 1
    in (newpos,newfacing,newnobes,newinfeectioncount)

initialpos = let n = div (length sample) 2 in (n,n)
initialfacing = U

initialmap = M.fromList $ concatMap mapifyRow $ zip [0..] $ map (zip [0..]) sample
    where mapifyRow (y,row) = map (\(x,c) -> ((x,y),if c == '#' then Inf else Clean)) row

getcount(_,_,_,x) = x
main = do print $ initialpos
          print $ iterate burst (initialpos,initialfacing,initialmap,0) !! 2
          print $ getcount $ iterate burst (initialpos,initialfacing,initialmap,0) !! 100
          -- print $ getcount $ iterate burst (initialpos,initialfacing,initialmap,0) !! 10000000

sample = [
    "..#",
    "#..",
    "..."]


input = [
    "..##.##.######...#.######",
    "##...#...###....##.#.#.##",
    "###.#.#.#..#.##.####.#.#.",
    "..##.##...#..#.##.....##.",
    "##.##...#.....#.#..#.####",
    ".###...#.........###.####",
    "#..##....###...#######..#",
    "###..#.####.###.#.#......",
    ".#....##..##...###..###.#",
    "###.#..#.##.###.#..###...",
    "####.#..##.#.#.#.#.#...##",
    "##.#####.#......#.#.#.#.#",
    "..##..####...#..#.#.####.",
    ".####.####.####...##.#.##",
    "#####....#...#.####.#..#.",
    ".#..###..........#..#.#..",
    ".#.##.#.#.##.##.#..#.#...",
    "..##...#..#.....##.####..",
    "..#.#...######..##..##.#.",
    ".####.###....##...####.#.",
    ".#####..#####....####.#..",
    "###..#..##.#......##.###.",
    ".########...#.#...###....",
    "...##.#.##.#####.###.####",
    ".....##.#.#....#..#....#."]
