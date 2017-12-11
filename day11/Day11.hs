import Data.List
import Control.Arrow

data Dir = N | NE | SE | S | SW | NW

step (x,y) N  = (x,  y+2)
step (x,y) NE = (x+1,y+1)
step (x,y) SE = (x+1,y-1)
step (x,y) S  = (x,  y-2)
step (x,y) SW = (x-1,y-1)
step (x,y) NW = (x-1,y+1)

dist (x,y) = div (abs x + abs y) 2

main = print $ (last &&& maximum) $ map dist $ scanl step (0,0) input

input = [
    NE, N, NE, S, NW, S, S, SW, SW, SW, SW, SW, NW, NW, SW, NW, NW, N, NW, NW,
    NW, NW, NW, S, N, NW, S, N, N, NW, N, N, SE, N, N, N, S, N, N, N, N, SW,
    SE, N, N, NE, S, NE, NE, NW, NE, N, NE, NE, NE, NE, NE, S, NE, NE, S, SE,
    NE, NE, NE, NE, NE, NW, NE, SE, NE, NE, N, NE, NE, SE, NE, NE, SE, SE, SE,
    SE, SE, N, SE, NE, SE, SE, NE, NE, SE, SW, SW, SE, SE, S, SW, SE, SE, SE,
    S, N, SE, SE, S, SE, S, SE, SE, SE, SE, SE, SE, N, S, S, NW, SE, S, S, NW,
    S, SE, SE, S, S, S, SW, S, S, SE, N, NE, S, S, S, S, S, S, NW, S, N, S, N,
    SE, S, S, SW, S, NE, S, S, S, S, S, NW, NE, S, S, SW, S, S, S, S, SE, N,
    NW, S, S, S, S, S, SW, S, N, SW, S, S, SW, SW, S, SW, SW, NE, SW, SW, S,
    NE, S, SW, SW, SW, SW, S, SW, SE, SW, SW, S, SW, SE, S, SW, SW, SW, SE, SW,
    SW, SW, SW, S, SW, S, SW, S, SW, SW, NE, SW, SW, NE, SW, SW, S, SW, SW, SW,
    NE, SW, SW, SW, SW, SE, SE, SW, NW, SW, SW, NW, SW, NW, NW, SW, SW, SW, SW,
    NW, SW, NW, SW, SW, NW, SW, N, SW, NW, SW, S, S, SE, NW, S, NW, SW, NW, NW,
    SW, NW, S, NW, SE, S, SW, SE, NW, NW, N, SW, SW, NW, SW, NW, NW, NW, NE,
    SW, NE, SE, SW, SW, NE, NW, SW, NW, NW, NW, NE, NW, NW, SW, NW, SW, NW, SW,
    NW, NW, NW, NW, NW, NE, NW, NW, NW, NW, NW, NW, NW, NE, NW, NW, NW, NW, NW,
    NW, NW, NE, NE, NW, S, SW, NW, NW, NW, SE, NW, SE, SE, NW, NW, NW, N, NW,
    NW, NE, NW, NW, NW, NE, NW, N, SW, NW, NE, NW, SW, NW, N, NW, S, NW, N, NW,
    NW, NW, NW, NW, N, N, SW, NW, NW, N, NW, N, NW, NW, NW, NW, SE, N, NW, NW,
    NW, NE, NW, N, NW, N, NW, N, NW, NW, NW, NW, N, NW, SW, NW, N, N, SE, N, N,
    N, NW, NW, N, NW, N, NE, NW, NW, NW, NW, N, S, NW, S, NW, NW, N, NW, N, N,
    N, N, S, NW, NW, N, N, SE, NW, NW, NW, S, N, N, SW, SW, N, N, NW, N, N, N,
    NW, SW, N, NE, S, N, N, N, N, NW, N, NW, N, N, N, SW, N, N, N, N, NW, N, N,
    N, N, N, NE, N, SE, N, N, N, N, N, N, NE, N, N, N, SE, N, N, N, N, N, NE,
    N, N, N, NE, NW, N, N, N, N, N, NE, NE, N, N, N, N, N, N, N, N, N, N, N,
    NW, SE, N, NE, N, SW, N, N, N, N, NE, NE, N, NE, N, N, S, N, SW, NW, N, NE,
    NE, NW, S, SE, NE, N, NE, N, N, NE, SE, N, SW, NE, N, N, SE, S, NE, NE, N,
    N, N, NE, NE, N, SE, S, N, N, N, N, N, NE, S, NW, NE, N, N, N, NE, NE, SW,
    NE, NE, N, NE, NE, NE, NE, SW, NE, N, N, NE, N, NE, N, N, N, S, N, N, NE,
    N, NE, N, NE, NE, NE, NW, NE, SW, S, NE, N, NE, NE, NE, N, N, NE, NE, NE,
    SE, NW, NE, N, NE, NE, N, NE, NE, NE, NE, NE, N, N, N, NW, NE, NE, SE, N,
    N, N, N, NE, N, NE, NE, NW, NE, NE, N, NE, NE, NE, NE, NE, NW, N, NE, SE,
    NE, NE, NE, NE, NW, NE, NE, NE, NE, NE, NE, NE, SE, SE, NE, NE, NE, NE, NE,
    NE, NE, N, NW, NE, NE, NE, NE, NE, NW, NE, NE, NE, NE, NW, NE, NE, NW, SE,
    NE, NE, NE, NE, NE, NE, NE, NE, NW, NE, NE, NE, NE, NE, SE, NE, SW, NE, NE,
    SE, NE, SW, NE, NE, NE, NE, NE, NE, SE, NE, NE, NE, NE, NE, NW, NE, N, NE,
    S, NE, NE, SE, NE, N, NE, SW, SE, NE, NE, SW, SE, NE, SE, SE, NE, NE, SE,
    SE, NE, S, NE, S, NE, N, NE, NE, NE, NE, SW, NE, NE, SE, NE, NE, NE, SE,
    NE, SE, NE, NE, NE, SE, NW, NE, NE, NE, NE, NE, S, NE, S, SE, NE, NE, SE,
    N, NE, SE, SE, SE, NE, NE, NE, NE, NE, SE, SW, NE, NE, SE, SE, SE, NE, NE,
    SE, SE, SE, NE, SE, S, NE, SE, SW, NE, SE, NE, SE, NE, SE, NE, SE, SE, N,
    SE, SE, N, NE, SE, SE, SE, NE, N, NE, SE, NE, N, N, NE, S, NE, SE, N, NE,
    NE, SE, NW, SE, SE, NE, SE, NE, SW, S, N, SE, NW, NE, SE, SE, NE, NE, SE,
    SE, SE, SE, SE, NE, NE, NE, NE, SE, SE, SE, SE, NE, SE, NE, SE, SE, N, SE,
    SW, SW, SE, SE, SE, SE, SE, SE, SE, SE, NE, SE, NW, SE, SE, SE, NE, NE, N,
    SE, NE, SE, SE, NE, SE, S, N, S, NE, SE, SE, NE, SE, NE, SE, SE, SE, NE,
    SE, SE, SE, SE, NE, SE, SE, NE, SE, SE, NE, SE, SE, SE, SE, SE, NE, NE, NE,
    SE, SE, SE, SE, SE, N, SE, S, NW, SE, N, S, SE, SE, SE, SE, SE, SE, SW, SE,
    SW, SE, SE, SW, SE, NW, SE, SE, SE, SE, SE, NE, SE, SE, SE, NE, SE, SE, S,
    SE, SE, SE, SE, NW, S, NW, SE, SE, SE, SE, SE, S, SE, NE, SE, N, SE, SE,
    SE, SW, SE, N, SE, SE, SE, SE, SE, SE, SW, SE, SE, N, SE, SE, SE, SE, SE,
    N, SW, S, SE, SE, N, SE, SE, SE, SE, SW, SE, SE, SE, NW, SE, SE, SE, SE,
    SE, S, S, SW, SE, SW, SE, SE, S, SE, SE, SE, S, NE, SE, SW, SE, S, SE, SE,
    NE, SE, SE, SE, SE, S, SE, SE, SE, SE, S, SE, SE, S, S, S, SE, SW, SE, SE,
    SE, N, SE, SW, S, S, SE, SW, SE, S, S, S, SE, NE, SE, SE, SE, S, SE, SW, S,
    S, SE, SE, N, SW, S, N, SW, S, SE, SE, S, S, S, N, SE, SE, SE, S, SE, SE,
    SE, S, SE, SE, SE, SE, NW, SE, N, SE, S, SE, SE, S, SE, S, NW, SE, S, NE,
    SE, SE, S, SW, SE, S, N, SE, N, S, SE, SE, SE, NE, SE, S, S, S, NE, SE, SE,
    SE, SE, N, SE, S, SE, SE, S, SE, N, SE, SE, S, N, SE, S, S, SE, S, NW, SE,
    S, S, SE, SE, S, S, S, NW, SE, SE, SE, SE, S, NW, SE, S, SE, S, SE, SE, S,
    SE, S, S, N, S, S, SE, S, NW, S, SE, S, SE, S, S, NE, SE, S, S, N, SE, S,
    S, S, N, SE, SE, S, S, NE, S, SE, S, S, S, S, SE, S, SE, SW, S, S, S, SE,
    NW, S, S, NW, SE, S, S, S, S, S, S, SE, SW, SW, NW, S, SW, S, S, S, S, SW,
    S, S, S, S, SW, S, S, S, S, S, S, SE, S, S, S, SE, S, SE, S, N, S, S, S,
    SE, S, S, S, S, NE, S, S, S, SE, S, S, S, S, S, S, S, S, SW, S, S, S, S,
    SE, S, S, NE, S, SE, S, SE, NW, S, NW, N, N, S, SE, S, S, S, N, S, S, SE,
    S, N, S, S, S, SW, S, NW, S, SW, NW, NE, S, S, S, S, S, S, S, S, S, SW, S,
    NW, S, S, S, S, S, S, S, S, NW, S, N, S, S, S, S, NE, S, S, SW, S, S, S, N,
    N, S, S, SW, S, S, SE, SW, S, S, S, NW, S, NW, SW, S, N, S, S, SE, N, S, S,
    S, S, SW, S, S, S, NW, S, S, S, S, S, S, S, S, S, S, S, NE, S, NW, S, S, S,
    SW, S, S, S, S, S, S, S, NW, N, N, S, SE, S, S, S, S, SW, S, S, S, S, S,
    NW, SW, S, S, SW, S, S, SE, S, S, S, S, S, S, S, SW, S, S, S, S, SW, S, S,
    S, S, N, S, NW, SW, S, S, S, S, S, S, S, SW, S, S, SW, S, SW, S, SW, S, SW,
    S, N, S, SW, S, N, SW, S, S, S, S, SW, S, S, S, NE, SW, S, S, S, S, S, S,
    S, SW, S, S, NW, NE, S, S, SE, SW, SW, S, S, N, S, S, S, S, S, S, SW, SW,
    NE, S, S, S, S, S, S, S, S, S, SW, S, S, S, SW, SW, SW, SW, S, S, SW, NE,
    SW, SW, S, S, S, S, NE, SW, S, SE, S, S, S, S, S, S, SW, S, SW, S, NE, SW,
    SW, S, S, N, SW, S, S, SW, SE, S, NW, S, SW, S, S, SW, S, SW, SW, S, SW,
    SW, SW, SW, S, SW, S, S, S, S, S, S, S, SW, S, S, SW, S, SW, N, S, S, S,
    SW, NE, S, SW, S, SW, N, N, SW, SW, SW, S, NW, NW, SW, SW, N, N, S, SW, SE,
    SW, SW, SW, SW, SW, S, S, S, SW, SW, S, S, SW, SW, N, S, S, SW, S, SW, SW,
    SW, NW, SW, S, SW, SW, SE, SW, S, SW, SW, SW, S, S, SW, S, S, S, S, S, NW,
    S, SW, N, S, SW, N, SW, NW, SW, SW, S, SW, N, S, S, N, SW, SW, S, NE, S,
    SW, S, SW, SW, S, SW, S, S, NW, S, S, NW, S, SW, N, S, SW, SW, SW, SW, SW,
    SW, S, SE, S, N, S, S, S, S, SW, SW, NE, NW, SW, S, NW, SW, NW, N, SW, S,
    SW, SE, SW, S, SW, N, S, S, SW, SW, N, SW, SW, SW, S, SW, N, SW, SW, SW, N,
    NW, S, SW, SW, S, SW, S, SE, SW, SW, N, SW, SW, SW, NE, SW, S, S, S, SW,
    SW, S, SE, SE, SW, SW, S, S, SW, NE, NW, SW, S, SW, SW, SW, S, SW, SW, SW,
    SW, SW, SW, N, SW, SW, S, S, SW, SW, SW, SW, S, SW, SW, S, SW, S, S, SW,
    SW, SW, SW, SW, SW, SW, SW, SW, SW, S, N, NW, SW, N, SW, SW, SW, SE, SW,
    NW, SW, SE, SW, SW, SW, NE, SW, SW, SW, SW, SW, N, SW, SW, SW, SW, S, SE,
    S, SW, SW, S, SW, SW, NW, SW, SW, SW, SW, S, SE, SW, SW, SW, SW, SW, SW,
    SW, SW, SW, SW, SW, SW, SW, NW, SW, SW, SW, SW, SW, SW, SW, SW, SW, N, SW,
    SW, SW, SW, NW, SW, NE, SW, SW, SW, SW, N, SW, N, SW, NW, SW, SW, NE, S,
    SW, SW, SW, SW, NW, SW, SE, SW, SW, SW, SW, SW, SW, SW, SW, NW, S, N, NW,
    NW, SW, NW, SW, SW, SW, NW, SW, SW, N, SW, SE, NW, SW, NE, SW, SW, SW, NW,
    SW, SW, SW, SW, SW, SE, SW, SW, SW, NW, N, NW, NE, SW, SW, SW, SW, SW, SW,
    SW, SW, NW, SW, SW, SW, SW, SE, SW, N, SW, SW, SW, SW, SW, SW, SW, S, N,
    SW, SW, SW, SW, SW, S, SW, SW, SW, SW, SW, SW, SW, NW, SW, SW, SW, NW, SW,
    SW, S, NW, SW, NW, SW, SW, NE, SW, NW, SW, SE, SW, NW, S, NW, SW, NW, SW,
    NW, SW, SW, SW, SW, SW, N, SW, NE, NW, NE, SW, SW, SW, SW, S, NW, SW, SW,
    SW, NE, S, SW, SW, SW, SW, SW, SW, SW, SW, SW, SW, SW, SW, SW, NW, SW, SW,
    SW, SW, SW, SW, SW, NW, SW, SW, NW, NE, SW, S, SW, SE, S, NW, SW, SW, SW,
    NE, SW, SW, N, SW, SW, SW, NW, NW, NE, SW, SW, SW, SW, SW, SE, SW, NW, SE,
    SW, SE, SW, SW, SW, SW, SW, NE, SE, SW, SW, S, SW, SW, NW, SW, SW, SW, SW,
    SW, SW, SW, NW, NW, NW, NE, NW, SW, SW, N, SW, SE, NW, SW, SW, N, NW, NW,
    SE, N, S, SE, SE, NE, SW, SW, NW, SW, SW, SW, SW, SW, NW, SW, NE, N, SW,
    SW, SE, SE, SW, NW, NW, NW, NW, SW, NW, NE, SW, NW, SW, NW, SE, NW, N, SW,
    SW, SW, SW, SW, NW, SW, SW, NW, SW, SW, NW, SE, S, SW, NE, NW, NE, NW, SW,
    SW, SW, N, SW, SW, NW, SW, SW, NW, SE, SW, SW, S, SW, NW, NW, SW, SW, NW,
    SW, SE, SW, SW, NW, NW, N, SW, NW, S, NW, SW, SE, S, NW, SW, SW, SW, SW,
    SW, NE, NW, SW, NW, NW, SW, NW, SE, NW, SE, S, SW, SW, NW, NW, SW, SW, SW,
    SW, SW, NW, SW, NW, NE, NW, NW, SW, SE, SW, NW, NW, SW, NW, SW, SW, SW, NW,
    NE, NE, SW, SW, S, SW, NW, S, NW, SW, NW, SW, SW, SW, SE, SE, SW, SW, SW,
    SW, SW, SW, SW, SW, SE, N, NW, NW, NW, SW, SW, N, SW, S, SW, NW, NW, NW, S,
    SW, SW, N, SW, N, NE, NW, SW, NW, NW, SW, SW, S, SW, SW, NW, NW, NW, SW,
    SW, NW, NW, NW, NW, SW, SE, SW, NW, NW, SW, SW, NW, SW, SW, SW, SW, NW, NW,
    NE, SE, NW, N, SW, NW, SW, SE, SW, NW, NE, NW, SW, SW, NW, NW, NW, SW, NW,
    NW, NW, SW, NW, SW, NE, NW, S, SW, NW, NW, NW, SW, SW, N, NW, N, NW, NW,
    SW, S, NW, NW, SW, NW, NW, SW, NW, NW, SW, NW, SW, NW, SE, SW, NW, SW, SW,
    SW, SW, NW, SW, NW, SW, NW, NE, NW, NW, NW, NW, SE, SW, SW, NW, SW, NW, SW,
    SW, NW, NW, NW, SW, SW, SW, S, NW, SW, N, SW, SW, NW, NW, NW, SW, SW, NW,
    NW, SW, SW, NW, NW, SE, NE, NW, SE, S, NW, S, NW, SW, SW, SW, NW, SW, NW,
    N, NW, SW, SW, NW, NW, NW, NW, NW, NW, SW, NW, SW, NE, NW, NW, S, SE, NW,
    SW, SW, NW, NW, NW, NW, NW, SW, NW, SW, NE, NE, NW, NW, SW, S, NW, NW, NW,
    NW, NW, NW, SW, NW, NW, NW, NE, NW, SE, NW, N, NW, SW, SW, SW, SE, NE, NW,
    SW, SW, NW, NW, NW, NW, SW, SW, S, N, NE, NW, NW, NW, SW, NW, SW, NW, NW,
    NW, NW, NE, NW, NW, NW, NW, NW, SE, NW, S, NW, SW, N, SW, SW, NW, SE, NW,
    NW, NW, SW, NW, NW, SW, SW, SW, SW, NW, NW, N, NW, NW, NE, NW, NW, NW, S,
    NW, SW, NE, NW, NW, NW, NW, NW, NW, NW, NW, S, NW, SW, SE, N, NW, NW, SW,
    NW, N, NW, NW, NW, SW, NW, N, SW, NW, NW, NW, NW, SW, NW, SW, NW, NW, S, S,
    NW, NW, NW, N, NW, NW, NW, NW, SW, SE, NW, SW, NW, NW, NW, NW, SE, NW, N,
    SE, NW, N, NW, NW, S, SW, NW, NW, NW, N, NW, NW, NW, NW, NW, NW, NW, NW,
    NW, NE, NW, N, N, SW, NW, SW, NW, NW, NW, NE, NW, NW, S, NW, NW, NW, NW,
    NW, NW, SW, NW, SW, NW, NW, NE, SW, NW, S, NW, NW, NE, NW, NW, NW, NW, NW,
    NW, NW, NW, NW, NW, NW, NW, NW, NW, NW, NW, NE, S, NW, NW, SE, SW, NW, NW,
    NW, S, NW, NW, NW, NW, NW, NW, NW, NW, NW, NW, NW, NW, SW, NW, NW, NW, NW,
    NW, SE, NW, NE, S, SW, NW, N, NW, NW, NW, SE, NW, NW, SW, NW, SW, NW, NW,
    N, NE, NW, NW, NW, NW, NW, NW, NW, S, N, SW, S, NW, NE, S, NW, NW, SW, NE,
    N, NW, NW, NW, NW, NW, NW, SW, S, NW, NW, SW, NW, N, NW, NW, N, NW, NW, NW,
    NW, NW, N, SW, NW, NW, NW, SW, S, NW, SE, NW, NW, NW, S, NW, NW, NW, NW,
    NW, NW, NW, NW, NW, NW, N, NW, NW, SW, NW, N, NW, NW, NW, N, NW, NW, NW,
    NW, NW, NW, NW, NW, NW, NW, NW, NW, NW, NW, N, NW, N, N, NW, SE, NW, NW,
    NW, N, NW, NW, NW, SW, NW, NW, N, NW, NW, NW, NW, S, NW, S, NW, NW, S, NW,
    NE, S, NW, NW, NW, S, N, NW, NW, NW, NW, NW, NW, N, NW, NW, NW, SW, NW, N,
    NW, NW, NW, NW, S, N, NW, SE, NW, NW, N, NW, NW, N, NW, NW, NW, N, N, NW,
    NE, N, NW, SE, NW, NW, SE, NW, NW, NW, N, SE, NW, NW, NW, NW, NW, NW, NW,
    NW, N, SE, NW, NW, NE, N, NW, N, NW, SE, NW, N, NW, N, NW, NW, NW, NW, NW,
    SE, NW, NW, NW, NW, NW, NE, NW, N, N, NW, NW, SW, SE, NW, NW, NW, NW, NW,
    NW, S, S, SE, N, NW, N, NW, SE, NW, NW, NW, NW, NW, SW, N, NW, NW, NW, NW,
    N, S, N, NW, NW, N, NW, NW, N, NW, NW, NW, NW, NE, NW, NW, NW, NW, NW, NW,
    NW, NW, SW, NW, NW, NE, S, NE, NW, NW, NW, NW, SW, NW, N, NW, NW, SE, N,
    NW, NW, N, NE, NW, NW, N, N, NW, N, N, NW, NW, NW, NW, NE, NW, NW, NW, N,
    NW, N, N, NW, N, NW, NE, NW, NW, NW, N, NW, SE, N, NW, NE, N, NW, N, NW,
    NW, NW, NW, S, N, N, NW, N, SE, NW, NW, NW, NW, NE, N, NW, NW, NW, N, NW,
    NW, S, NW, N, SW, NW, NW, S, N, N, NW, NW, NW, N, N, SW, N, SE, NW, NW, NW,
    NW, S, NW, N, NW, NE, NW, NW, NW, NW, S, SW, N, NW, N, NW, NE, NW, NW, N,
    N, NW, N, SW, N, NW, NW, NW, NW, NW, N, NW, NW, N, N, N, S, N, SW, NE, N,
    NW, NE, NW, N, N, NW, NE, SE, NW, NW, S, NW, N, SW, NW, SW, N, NW, NE, NW,
    NW, NW, NW, S, NW, N, S, NW, N, N, NE, SW, NW, NW, NW, NW, N, N, NW, SW, N,
    N, NW, NW, NE, N, S, NW, N, N, NW, SE, N, N, N, NW, N, NW, N, NW, NW, NW,
    NW, N, N, NW, N, N, N, N, NW, N, NW, NW, NW, SW, NW, N, NE, N, SE, NE, N,
    NW, NW, N, N, S, N, N, NW, N, NE, S, N, NW, SW, SE, N, N, NW, NW, N, N, N,
    N, N, S, N, N, N, SW, N, S, NW, N, N, NW, NW, N, SW, N, NW, NW, NW, N, NW,
    N, NW, NW, NW, NW, SW, NW, NW, S, SW, S, N, N, NW, S, NW, NW, SE, NW, NW,
    NW, N, SE, N, N, NW, NW, N, N, NE, N, SW, S, N, NW, NW, NW, N, S, N, NW,
    NW, NW, N, N, N, N, N, NW, NW, NW, NW, NW, N, NW, NW, SE, N, N, SE, NW, NE,
    N, SW, NW, N, N, SE, S, SE, N, NW, SW, NW, N, S, NW, N, NW, NW, NW, SE, NW,
    NW, NW, S, N, N, NW, S, N, N, NW, N, N, N, N, N, N, NW, N, N, NW, N, N, N,
    N, N, NW, S, SW, NW, NE, N, N, NW, NW, NW, N, NW, NW, N, N, N, NW, N, NW,
    NW, N, N, N, NW, N, NW, N, NW, NW, NW, N, N, NW, NW, NW, N, NE, S, NW, NW,
    NW, NW, NW, N, N, NW, N, NW, N, N, NW, NW, N, NW, NW, N, SE, N, N, NW, N,
    NE, NW, N, NE, NE, SW, N, N, N, N, NW, N, SE, NW, NW, N, SE, N, NW, NW, N,
    S, N, N, NW, NW, NW, SW, N, NE, S, N, N, N, NW, NW, N, SW, N, NW, NW, N, N,
    NW, N, NE, N, NW, NW, NW, NW, NW, N, N, N, NW, N, N, N, NW, NW, N, N, N, N,
    N, N, NW, NW, S, NW, N, N, N, N, NW, NW, N, N, SW, N, SW, N, N, NW, N, SW,
    N, N, N, NW, NW, N, NE, N, N, NW, SE, S, N, N, N, N, N, N, N, N, N, N, N,
    N, N, N, SE, SE, NE, N, N, N, N, N, NW, NW, N, N, NE, N, NW, N, N, SW, N,
    NW, N, N, S, SE, N, N, N, NW, N, N, N, N, N, N, NW, N, N, SW, S, N, S, N,
    N, N, N, SW, N, NW, N, N, SE, N, N, N, SW, N, NE, N, N, N, N, N, N, N, N,
    N, N, N, N, N, N, N, NW, N, N, N, N, N, N, NW, N, N, N, N, NW, N, N, N, NW,
    NW, NW, N, NW, N, N, SE, NE, N, N, NW, N, SW, N, N, N, N, N, S, NW, N, N,
    N, N, N, NW, NW, SW, N, N, SW, SE, N, N, N, N, N, N, N, N, N, N, N, N, N,
    NE, N, N, NW, NW, SW, N, N, N, S, SE, N, SW, N, N, N, N, NE, N, NW, N, N,
    N, N, SW, N, SW, N, N, N, SE, N, NW, NW, S, NE, N, N, N, S, N, N, SE, N, N,
    N, NW, N, N, N, N, N, SW, N, S, N, N, S, N, N, N, N, NE, NW, N, N, N, NW,
    N, N, N, NW, NW, NW, N, N, NW, N, N, N, N, N, N, N, N, N, N, N, N, N, NE,
    NW, N, N, SW, NE, N, N, SW, NW, N, N, N, N, NW, N, N, N, N, N, N, NW, NW,
    N, N, N, N, S, S, N, N, N, N, N, N, NW, SW, N, N, N, N, N, SE, N, N, NE,
    SE, N, N, SE, N, N, N, N, NW, N, S, N, N, S, N, N, N, N, SW, N, N, N, N, N,
    N, N, N, N, NW, N, N, N, N, N, SE, N, N, SE, N, N, N, N, N, N, N, N, N, SE,
    N, N, N, N, N, S, N, N, N, N, N, NE, N, N, N, N, N, NE, N, N, NW, N, N, N,
    S, N, N, N, N, NE, N, SE, NE, N, NW, N, N, SW, S, N, NW, N, N, NE, N, SW,
    N, NE, N, N, N, N, S, SW, N, N, N, SW, N, NE, N, N, N, N, S, NW, N, N, N,
    N, N, N, NE, N, SE, NE, N, SE, N, N, S, NW, N, N, N, NW, N, N, N, NE, N, N,
    N, N, N, N, NE, N, N, NW, N, N, SW, SW, S, N, N, N, N, N, NE, N, SW, N, N,
    N, N, N, N, N, SE, NE, NW, N, N, N, N, N, N, N, SE, N, N, NW, NE, N, N, N,
    NE, NE, NW, N, N, N, N, NE, N, N, NW, N, NE, N, N, N, NE, N, N, N, NE, N,
    N, N, NW, N, S, NE, N, NE, N, N, N, N, NW, SE, SE, N, NE, N, N, NE, N, SE,
    N, N, N, N, N, SE, S, N, N, N, N, N, N, N, N, SW, N, N, N, N, N, N, N, N,
    N, N, N, N, NE, SW, SE, N, SE, N, N, N, SE, NE, NE, N, N, N, SE, NW, N, S,
    NE, N, N, S, N, N, N, N, NE, NE, SW, N, N, N, N, N, N, N, N, N, SE, N, NW,
    N, N, N, NE, N, S, N, NW, N, NW, N, NE, N, NE, N, NE, N, N, NE, NE, N, S,
    N, NE, N, N, N, SW, N, N, N, SE, N, N, N, N, NW, N, N, SW, N, N, N, N, N,
    N, SW, N, N, SW, N, N, NE, N, N, N, N, N, NE, N, N, N, N, NW, N, SW, N, N,
    N, N, NE, N, N, N, SW, N, N, N, N, N, N, NE, NE, N, N, N, NE, SE, SW, SW,
    N, N, N, N, NE, S, SE, N, N, SW, NE, SW, N, NW, N, SW, N, N, SW, N, NE, NE,
    N, N, NE, NE, SW, N, S, N, N, N, NE, NE, N, N, N, N, N, NE, N, NE, NW, NE,
    N, N, NE, N, NE, N, N, N, N, NE, N, N, N, N, N, N, N, N, N, N, N, N, NE,
    SW, SE, N, N, N, NW, N, NE, S, SW, N, NE, SW, N, N, NE, N, SW, N, N, NE,
    NE, N, NE, N, N, NE, NE, N, N, N, N, NE, NE, NE, N, N, NE, NW, N, N, N, SE,
    SW, NE, NE, N, N, N, N, NE, N, NE, NW, N, S, N, N, SE, N, N, N, NW, NE, NE,
    N, S, SE, NE, SE, NE, NW, NE, N, N, N, N, NE, NE, N, N, N, NE, NW, SE, NE,
    S, NE, SW, N, N, NW, NE, SW, NE, N, NE, NE, NE, N, N, SE, NW, NW, NE, NE,
    NE, NE, NE, SW, NE, N, NE, SE, N, SW, NE, NE, NE, NE, N, N, NE, N, NE, N,
    NW, NE, NE, NE, NE, SE, N, SE, NE, NE, NE, N, N, N, N, SW, N, N, SW, NE,
    NE, NE, N, S, NE, N, N, NE, NE, NE, SW, N, N, NE, N, SW, NE, S, N, N, NE,
    N, NE, N, NE, N, SW, NE, N, NE, N, N, N, N, N, N, NE, NE, N, S, NE, S, N,
    N, N, N, N, NE, NE, NE, NE, SW, NE, S, N, NE, SE, NE, NE, NE, NE, NE, NW,
    SW, NE, NW, SE, N, NE, N, N, N, N, NE, NE, NE, S, NE, N, NE, N, N, N, N, N,
    NW, NE, NE, N, S, N, NE, SE, N, NE, N, N, N, NE, NE, N, NE, N, NE, NW, N,
    NE, N, N, NE, N, N, NE, N, N, N, NE, NW, N, NE, N, NE, N, NE, N, N, NE, S,
    NE, N, N, N, NE, NE, N, N, NE, N, N, NE, NE, N, N, NE, NW, NW, NW, N, N, N,
    N, N, N, NE, N, NE, NE, N, N, N, SE, N, N, N, S, N, N, N, N, NE, N, N, N,
    NE, NE, NE, N, NE, NE, N, NE, N, N, NE, NE, NE, NE, N, NE, NE, N, N, S, NE,
    N, NE, N, N, N, NE, NE, N, NE, NE, SW, NE, NE, N, NE, NW, N, NE, N, N, NE,
    N, NW, S, NE, SE, NE, NE, NE, N, SE, NE, SE, SE, NE, NE, NE, NE, N, NE, N,
    NW, N, N, N, N, N, S, N, NE, N, SW, S, N, NE, N, NE, NE, N, N, NE, N, SE,
    SE, NE, NE, NE, NE, NE, NE, NW, NW, N, NE, N, N, N, NE, NE, N, N, N, NE,
    NE, NE, NE, NE, N, NE, NE, N, N, NE, N, S, SW, N, NE, NE, N, N, N, SW, N,
    NE, NE, NE, N, NE, NE, NE, NE, NE, NE, NE, N, NE, S, SW, N, N, N, NE, N,
    NE, NW, SE, N, N, N, N, N, NE, N, NE, N, N, S, SW, NE, N, NE, N, N, N, N,
    N, NE, NE, N, N, NE, N, S, N, NE, NE, SE, NE, N, SE, N, N, N, NE, NE, N, N,
    NE, NE, NE, N, N, NE, NE, N, NE, NE, NE, SW, NE, NE, NE, NE, S, S, N, NE,
    N, NE, S, NE, NE, S, NE, SE, NE, NE, NE, NE, N, N, NE, NE, NE, SE, SE, NE,
    N, NE, N, S, N, SE, NW, NE, SW, NE, N, S, NE, NE, NE, SW, N, NE, NE, NE,
    NW, N, SE, NE, NE, NE, S, S, N, NE, NE, N, NE, SE, NE, NE, N, N, N, N, NE,
    NE, NE, NE, NE, NE, NW, S, SW, SW, NE, NE, SW, NE, NE, N, SE, NE, NE, SW,
    N, NE, S, NE, NE, N, N, NE, SW, NE, NE, NE, NE, NE, N, NE, NE, NE, NE, NE,
    NE, N, NE, NE, NE, N, NE, NE, S, NE, NE, NE, SE, NE, S, NE, NW, N, NE, NE,
    NE, S, NE, NE, NE, NE, NE, NE, NE, N, NE, N, N, N, NE, SE, N, NE, NE, N, N,
    NE, N, N, NE, NE, NE, N, NE, NE, NE, NE, N, N, N, NE, NE, NE, NE, NE, NE,
    NE, N, SE, N, NW, NE, S, SW, N, NE, N, N, NE, N, NE, NE, NE, NE, NE, NE,
    NE, N, SE, NE, NE, NE, SE, SE, NE, NW, NE, NE, NE, NE, N, NE, NE, NE, NE,
    N, N, NE, S, N, SW, NW, N, NW, NE, NE, N, NE, NE, NE, NE, S, NE, N, NE, NE,
    NE, NE, NE, NE, N, NE, N, NE, SE, NE, NE, N, NE, N, NE, N, NE, NE, NE, NE,
    SW, NE, NE, NE, NE, NE, N, NE, SW, NE, NE, NE, NE, NE, NE, S, N, NE, N, NE,
    NE, NE, NW, NE, NE, NE, N, N, NE, S, N, N, N, NE, NE, NW, N, NE, N, SW, NE,
    NE, NE, N, NE, NE, NE, NE, S, N, NE, NE, NE, NE, NE, NE, NE, NE, NE, NE, N,
    N, SE, NE, N, NW, NE, S, NE, NE, NE, SW, N, SE, NW, SE, NE, NE, NE, NE, SE,
    N, NW, NE, NE, NE, NE, S, NW, NE, NE, NE, S, NE, SE, NE, NE, NE, NE, NW,
    NE, NE, N, NE, NE, NE, NE, N, NE, NE, NE, NE, SE, NE, NE, NE, NE, NE, NE,
    NE, NE, NE, NE, NE, S, NE, NE, N, NE, NE, NE, NE, N, NE, SW, NE, NE, NE,
    NE, S, SE, NE, NE, N, N, NE, NE, NE, NE, SW, SW, NE, S, NE, NE, NE, NE, NE,
    N, N, N, NE, NE, NE, NE, NE, NE, NE, NE, NE, NE, NE, NE, NE, NE, NE, SW,
    SW, NE, N, NE, NE, N, NE, NE, NE, SW, NE, NE, NE, NE, N, NE, NE, NE, NE, N,
    NE, N, SE, NE, S, NW, NE, SW, SW, N, N, N, NE, NE, N, NE, SW, NE, NE, N,
    NE, N, NE, NE, NE, NE, NE, S, SW, N, NE, NE, NE, SE, NE, S, NE, NE, S, S,
    NE, NE, S, NE, N, NE, N, N, NE, N, NE, NE, NE, NE, NE, SW, NE, SW, NE, NE,
    NE, NE, NE, NE, NE, NE, NE, NE, NE, NE, NE, SE, N, NE, NE, NE, S, NE, NE,
    NE, NE, NE, NE, NE, NE, SW, NW, NE, NE, NW, NW, N, NE, NE, SW, NE, NW, NE,
    NE, N, NE, NW, SE, NE, NE, NE, NE, S, NE, NE, N, NE, NE, SE, N, NE, N, NE,
    NE, NE, NE, NE, NE, NE, NW, NE, NE, SW, NE, N, N, SW, NE, NE, NE, NE, NE,
    NE, NE, NE, NE, NE, NE, S, NW, NE, NE, NE, NE, NE, NE, NE, NW, NE, NE, S,
    NE, NE, NE, NE, NE, N, NE, NE, NE, NE, NE, NE, S, NE, NE, NE, NE, NE, NE,
    S, NE, NE, NE, NE, NE, NE, NE, NE, NE, SE, NE, NE, NE, NW, NE, NE, NE, N,
    NW, NW, SW, SW, SW, SW, NW, S, SE, S, S, S, SE, NE, SE, N, SE, S, NE, S,
    NE, NW, SE, SE, NE, SE, NE, NE, S, SE, SE, NE, NE, NE, NE, NE, S, SW, NE,
    NE, N, NE, NE, NE, N, N, N, N, N, N, N, N, N, NW, N, N, N, NE, SW, SW, N,
    N, N, NE, N, S, SE, NW, N, NW, NW, N, N, SW, NW, S, N, N, SW, SW, N, NW,
    NW, NW, N, SE, NW, NW, SW, N, NW, N, NW, NW, NW, NW, NW, NW, NW, NW, NE, N,
    NW, N, NW, S, S, SE, NW, S, SE, SW, NW, S, NW, S, SW, NW, NW, NW, SW, NW,
    NW, NW, NW, SW, S, NW, NE, SW, NE, SW, SW, NW, SW, NW, SW, SW, NW, NE, SW,
    SW, NE, SE, NW, SW, SW, SW, SW, SW, S, SW, S, SW, SW, S, SW, S, SW, SW, S,
    NE, SE, S, SW, SW, SW, S, S, S, SE, S, SW, SW, S, S, S, NW, S, S, S, S, S,
    N, S, SW, NW, SW, S, SW, SW, SW, S, S, S, SW, SE, SE, S, S, S, S, S, S, S,
    S, S, S, S, SE, S, S, SW, S, S, S, SE, S, SE, S, S, S, S, S, S, SE, SE, SE,
    S, S, S, NW, S, SE, NE, SE, S, SE, S, SE, S, S, S, S, SE, NE, SE, S, S, S,
    S, S, NE, S, S, S, SE, S, S, NW, SE, NW, SE, S, SE, SE, S, SE, S, N, SW, S,
    SE, S, S, N, N, S, SE, SE, SE, NW, SE, SW, SE, S, SE, S, SE, SE, SE, S, SE,
    SE, S, SW, SW, N, S, SE, SE, SE, SE, SE, SE, SE, SE, NE, SE, SE, S, SE, SE,
    SE, SE, SE, SE, NW, S, SE, SE, SE, SE, SE, SE, SE, SE, NW, SE, SE, SE, SE,
    SE, SE, SE, S, N, SE, SE, SE, SE, SE, SE, NE, SE, SE, SE, SE, SE, S, SW,
    SE, SE, SE, S, SE, SE, SE, SE, NE, SE, SE, SE, SE, NE, SE, NE, SE, SE, SE,
    NE, NE, NE, SE, SE, NE, SW, SE, SE, NE, SE, SW, NE, NE, SE, SE, NE, SE, NE,
    SE, NW, N, NE, SE, SE, NE, SE, NE, SE, SE, NE, NE, S, SE, NW, NE, SE, SE,
    SE, SE, NE, NE, SE, SE, NE, NE, SE, SE, NE, S, NE, NE, SE, SE, NE, S, S,
    NE, NE, SE, S, NE, SE, NE, SE, NE, NE, NE, NE, N, NE, NE, SE, NW, SE, NE,
    NE, NE, NE, NE, NE, NW, NE, NE, N, NE, NE, NE, NE, NE, NW, N, NE, NE, S,
    NE, NE, NE, NE, N, NE, NE, NE, S, NE, NE, NE, NE, NE, NE, NE, NE, NE, NE,
    NE, NE, NE, N, NE, NE, SE, NE, NE, NE, N, SW, NE, NW, S, NE, N, NE, NE, NE,
    NE, NE, SW, NE, NE, NE, NE, NE, N, NE, NE, NE, NE, NE, NE, NE, SW, NE, NE,
    NE, S, N, NE, N, N, NE, NE, N, S, NE, N, NE, S, N, NE, NE, NE, NE, NE, NE,
    NE, N, NE, N, N, NE, S, NE, NE, N, NE, NE, NE, NW, S, N, SE, NE, NE, N, N,
    NE, N, SW, SE, NE, SW, NE, NE, NE, N, N, SE, NE, S, N, N, N, NE, NE, N, N,
    NW, NE, NE, N, N, NE, NE, NE, N, N, NE, N, N, N, N, N, NE, NE, NE, N, N, N,
    NE, N, N, SW, N, N, NE, NE, S, NE, SW, NE, N, S, N, SE, N, NE, N, N, N, N,
    NE, NE, N, N, N, SW, N, SW, N, NE, N, N, N, SW, N, N, N, S, NE, N, N, N, N,
    SW, N, SW, N, N, N, N, N, S, S, SE, N, N, N, N, N, NW, S, S, N, N, N, N, N,
    N, NW, N, N, N, N, N, N, SW, SE, N, N, N, N, N, NW, S, N, SW, SW, N, NW, N,
    NE, NW, S, N, N, N, S, NW, N, N, N, N, N, N, N, N, S, N, N, N, N, N, N, N,
    SW, N, N, N, N, N, N, NE, S, NW, SW, SW, NE, NW, N, N, N, N, NW, NE, N, N,
    NW, N, SE, N, N, NW, NW, N, SW, NW, N, N, NW, N, N, NW, N, N, N, N, NW, NW,
    NW, N, NW, NW, N, N, NW, N, N, NW, N, SW, N, NW, N, N, N, S, N, NW, N, N,
    S, N, N, N, N, NW, N, N, S, NW, NW, N, SW, NW, N, N, N, N, NW, N, NW, N, N,
    N, S, NE, N, N, SE, N, SE, N, N, NW, SE, N, NW, NW, NW, S, NW, N, NW, NW,
    N, NW, NW, SE, N, NW, SW, NW, NW, NW, NW, N, NW, NW, NW, NW, SW, N, N, NW,
    N, S, SE, N, N, N, NW, S, NW, N, NW, N, N, NW, NW, NW, N, NW, N, NW, N, N,
    S, N, NW, NW, N, NE, N, N, N, N, NW, S, NW, N, NW, NW, NW, NW, NE, NW, N,
    N, N, NW, NW, N, NW, NW, N, NW, NW, N, N, N, N, NW, NW, NE, NE, NE, NW, N,
    SW, NW, N, NW, NW, NW, NW, NW, NW, NW, NW, NW, NW, NW, S, NW, NE, NW, SE,
    NW, NW, NW, NE, SE, NW, NW, NW, NW, N, NE, NW, NW, NW, NW, NW, NW, NW, NW,
    NW, NW, NE, NW, NW, NW, NW, N, NE, NW, NW, NW, NW, NW, SE, NW, NW, NW, NW,
    S, SW, NW, NW, SE, NW, NW, NE, NW, NW, NW, NW, NW, SW, NW, SW, NE, NW, SE,
    NE, NW, NW, SW, SW, NE, S, NW, SE, NW, SW, NW, NW, NW, NW, NW, SE, NW, NW,
    NW, SW, NW, NW, SW, NE, NW, SW, NW, NW, NW, S, S, NW, NW, NW, N, SE, NW,
    NW, NW, NW, NW, NW, NW, SW, SW, NE, S, N, NW, NW, SE, NW, NW, SW, NW, SE,
    NW, NW, SE, S, SE, SW, SW, NW, NW, NW, N, NW, NW, NW, SE, NW, SW, NW, NW,
    SW, NW, SW, NW, N, NW, SW, NE, SE, NW, NW, S, SW, NW, N, SW, NW, SW, SE,
    NE, SW, NW, NW, SW, S, NW, SW, NW, N, SW, NW, NE, NE, NW, S, NW, NW, NW,
    NW, SW, NW, SW, SW, NW, SW, NW, NW, S, NW, NW, NW, NW, SE, NW, NW, SW, NW,
    N, NW, NW, NW, NW, SW, NW, NW, SW, NW, SW, SW, NW, SW, NW, NW, SW, N, SW,
    NW, SW, NW, NW, NW, SW, SW, SW, SW, NW, SW, SW, NW, NW, SW, S, SW, N, SW,
    NW, NW, SW, N, SW, SW, NE, NW, SW, S, SW, NE, NW, NW, N, SW, NW, SW, SW, S,
    N, SW, SW, NW, SE, SW, NW, NW, NE, SW, NW, NE, SW, NW, SW, SW, NW, N, SW,
    NE, SW, N, S, SW, SW, SW, SW, SW, NW, SW, S, N, SW, NW, NE, SW, SW, SW, NW,
    SE, SE, NW, SW, SE, SW, N, SW, SW, NE, SW, S, NW, SW, SW, SW, SW, SE, NW,
    N, SW, SW, SW, SW, SE, SW, NW, SW, NW, S, NW, SW, SW, NE, NW, SW, SW, SW,
    SW, SW, S, NW, SW, S, SW, NW, SW, NW, SW, SE, SW, N, NW, SW, SW, SW, SW,
    NW, SW, SW, SW, NW, S, NW, SW, N, SW, N, SW, SW, SW, NW, SW, SW, SW, SW,
    SW, SW, SW, SW, S, NW, N, SW, SW, SW, SW, SW, N, NW, SW, S, SW, SW, SW, SW,
    SE, NW, SW, SW, NW, SW, SW, SW, SW, SW, SW, NW, SW, SW, SW, SE, SW, SW, SW,
    SW, SW, SW, SW, NW, S, SW, SW, SW, SW, NW, SW, SW, SW, N, SW, NE, NW, N,
    SW, SW, SW, SW, SW, SW, SW, SW, SW, SE, SW, SW, SE, SW, SW, NW, NW, SW, SW,
    NE, SW, SW, S, SW, NW, SW, SW, SW, NE, NE, SW, NE, SW, SW, SW, SW, SW, S,
    SE, SW, S, NW, SW, SW, NE, SW, NW, SW, S, SW, SW, SW, S, SW, SW, N, SE, SW,
    SW, SW, SW, SW, SW, S, SW, NE, SW, NE, S, SW, N, SW, S, N, SW, SW, SW, SW,
    NW, SW, SW, S, SW, NE, NE, SW, SW, SW, SW, SW, SW, SE, SW, SW, SW, N, SW,
    SW, SW, SW, SW, SW, NE, SW, S, S, SW, SW, SW, SW, SW, SE, SW, N, NE, SW,
    SW, NW, S, NE, SW, SW, SW, S, SW, SW, SW, SW, SW, SW, SE, NE, SE, SE, SW,
    SW, NW, S, SW, SW, SW, SW, SW, NE, SW, SW, N, SW, SE, S, SW, SW, N, SW, SW,
    SE, SW, SW, SW, S, S, NW, NE, S, SW, SW, S, S, N, N, S, SW, S, SW, S, S, S,
    SW, SE, S, S, N, SW, NW, SW, SW, SW, SW, SW, SW, SW, SW, S, SW, SW, S, NE,
    NE, SW, SE, S, SW, S, SW, S, NW, SW, S, S, S, S, NW, SW, S, NW, SW, S, S,
    NE, S, S, SW, S, SW, SE, SW, SW, S, SW, N, N, S, S, SW, SW, NE, NE, SW, S,
    S, SW, SW, S, S, SW, S, S, S, S, S, S, NE, SW, SW, SW, SW, S, SW, S, S, N,
    S, SW, SW, S, NW, S, SW, SW, S, SW, NW, SW, SW, SW, SW, N, S, SW, SW, NE,
    S, NE, S, SW, SW, SW, SW, S, SW, SW, S, S, S, SW, S, S, SW, S, S, S, SW,
    SE, NW, SE, S, SW, S, SW, S, SE, SW, S, S, SW, S, SW, SW, S, SW, S, SW, SW,
    SW, SW, SW, S, SW, S, S, NE, SW, S, S, S, SW, S, S, S, S, S, S, S, N, S,
    NW, SE, S, SW, S, SW, S, SW, SW, SW, NE, NW, SW, SW, S, S, SW, S, S, S, S,
    NE, SE, S, S, S, SW, S, SW, SW, S, S, S, SW, S, S, S, S, S, SW, S, S, S, S,
    N, S, S, S, NE, S, S, SW, SW, S, S, S, NE, S, N, S, SW, S, SW, SW, S, S,
    SW, SW, SW, SW, SW, SW, S, N, N, S, NE, NE, SW, NW, S, S, S, NW, S, S, S,
    NW, NE, S, S, S, S, S, N, NE, S, S, SW, S, S, S, S, S, NE, SW, S, S, SE, S,
    SW, N, N, S, SW, NE, NE, S, SE, S, S, NE, S, S, S, SW, SE, S, S, S, SW, S,
    S, S, NW, S, SE, S, S, S, NW, SW, S, S, SW, S, S, S, S, S, SW, SE, S, S, S,
    S, S, S, N, NW, S, S, SE, NW, S, S, S, S, S, SE, SE, S, S, N, S, S, NW, NW,
    N, SW, SE, S, S, N, S, S, S, S, SW, SW, NW, S, NE, S, N, S, NW, NE, S, S,
    S, S, S, S, S, S, S, SW, SW, S, S, S, S, SW, S, NW, N, S, S, S, S, S, S, S,
    S, S, S, SW, S, S, S, NE, N, NW, S, SW, S, S, S, S, S, NW, S, S, S, S, NW,
    S, S, N, S, S, S, S, NW, S, N, NW, S, N, S, S, S, NW, S, NW, S, S, S, S,
    SW, S, SW, S, S, S, S, S, S, SW, S, S, S, N, S, SW, S, S, S, N, NE, S, S,
    S, S, S, S, NW, S, S, S, NW, SE, S, S, SW, S, S, S, SW, S, SW, SE, N, S,
    SW, S, S, S, S, S, NE, SE, S, NW, S, S, N, N, S, S, S, S, S, SE, S, SE, S,
    S, SE, N, S, S, NE, S, S, S, S, S, S, S, S, S, S, S, SE, S, S, S, S, S, SE,
    S, S, SE, N, SW, S, S, S, S, S, S, S, S, S, S, S, SE, S, NW, S, S, N, S,
    NW, S, S, NW, S, S, N, S, SW, S, S, N, S, S, S, S, S, S, SW, S, S, NW, S,
    NW, SE, SE, SE, S, S, N, SE, S, S, N, S, S, N, SE, S, SE, NW, SE, SE, S, S,
    S, SE, N, SE, SE, S, SE, S, S, S, S, S, S, S, NW, SE, S, S, NW, SE, NE, SE,
    NE, S, S, S, S, S, N, SW, SE, SE, S, SW, SE, S, S, S, SE, S, SE, S, S, S,
    S, S, S, SE, S, S, S, S, S, NE, SE, SE, S, S, SE, SE, S, SE, S, S, S, NW,
    S, S, S, SE, S, SE, S, SE, S, N, SW, NE, SE, NE, S, SE, S, S, S, S, S, S,
    S, S, S, S, N, S, N, S, SE, NE, S, NW, S, S, SE, NW, SW, SE, SE, S, SE, S,
    S, SW, S, N, S, S, S, S, NE, SE, S, S, NE, S, NE, SE, SE, S, NE, SE, S, S,
    SE, SE, S, N, SE, S, N, SE, S, SW, SE, S, SE, S, S, SE, S, S, S, NW, S, SE,
    S, S, S, S, SE, S, S, S, S, NE, S, S, S, S, SE, NE, S, SW, SE, S, S, SW, S,
    SW, S, SW, SE, S, N, S, S, S, N, S, SE, SE, S, S, S, S, S, SE, SE, SE, S,
    SE, NE, S, SE, S, SE, S, S, S, SE, S, SE, SE, S, NW, S, N, S, S, S, S, S,
    SE, SE, S, NE, NW, N, SE, NE, S, SE, SE, SE, SW, SE, NE, SE, S, S, SE, S,
    S, SE, N, SW, NW, SE, NE, S, SE, S, S, SE, SE, S, S, S, N, S, S, N, S, SE,
    S, NW, SE, N, SE, SE, NW, SE, S, SW, SE, SE, S, S, SE, SE, SE, N, S, SE,
    SE, SE, NE, SE, SW, S, S, S, S, SE, S, S, SE, SE, NW, NW, S, N, SE, S, SE,
    SE, S, SE, S, SE, NW, NE, SE, SE, S, NW, S, S, S, SE, S, SE, SE, S, SW, SE,
    SE, S, SE, SE, SE, NE, SE, S, SE, S, S, S, SE, SW, SE, S, SE, NW, SE, NW,
    SE, N, N, S, S, S, SE, SE, SE, NE, S, SE, SE, SE, SW, S, SE, NW, SE, NW,
    SE, SE, S, SE, NE, S, SE, SE, SW, S, SE, SE, NW, S, S, SE, SE, SE, SE, S,
    SE, SE, S, SE, NE, SE, N, SW, SE, NE, SE, SE, SE, SE, SE, N, S, SE, SE, S,
    S, S, SW, SE, SE, S, S, SE, S, SE, SE, SE, SE, SE, SE, NE, S, S, S, SE, SE,
    SE, S, S, SE, SE, SE, NE, S, S, S, SE, SE, SE, SE, SE, SE, N, SE, S, S, SE,
    SE, SE, SE, SE, SE, S, SE, SE, SE, S, N, SE, SW, S, SE, SW, SE, SE, SE, S,
    NW, SE, S, SE, SE, SE, SE, S, SE, S, SE, SW, SE, NE, SE, S, SE, SW, SE, S,
    SE, SE, SE, SE, S, SE, NE, SE, NW, SE, SE, SE, SE, S, SE, S, SE, SE, NE,
    SE, S, SE, NW, SW, SE, NE, SE, N, SE, S, SE, S, SE, SW, SE, SE, S, SE, SE,
    SE, SE, SE, SE, SE, SW, S, SE, SE, SE, SE, SE, NE, SE, SE, NW, SE, NW, SE,
    NW, SE, SE, SE, SE, SE, SE, SE, SE, SE, SE, SW, SW, SE, SE, SE, N, SE, SE,
    N, S, SE, SE, N, SE, SE, SE, SE, SW, NW, SE, SE, SE, SE, SE, SE, SE, SE,
    SE, S, SE, SE, S, SE, SE, SE, S, SE, S, SW, SE, SE, SE, SW, SE, N, SE, SW,
    SE, S, SE, SE, SE, SE, NW, SE, SE, SE, SE, SE, SE, SE, SE, SE, SE, SE, SE]
