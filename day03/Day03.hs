import Data.List
import Control.Monad

inclaim x y [_,x0,y0,w,h] = x >= x0 && x < x0+w && y >= y0 && y < y0+h
numclaims x y  = length . filter (inclaim x y)

disjoint [_,x0,y0,w0,h0] [_,x1,y1,w1,h1] = (x0 >= x1+w1) || (x0+w0 <= x1) || (y0 >= y1+h1) || (y0+h0 <= y1)

main = do input <- map (map read . words) <$> lines <$> readFile "input.txt"
          print $ sum [ 1 | x <- [0..1000], y <- [0..1000], numclaims x y input > 1 ]
          print $ find (\c -> all (disjoint c) (input \\ [c])) input
