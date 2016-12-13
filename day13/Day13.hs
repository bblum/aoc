import Data.List
import Data.Bits

open (x,y) = x >= 0 && y >= 0 && (even $ popCount $ x*x + 3*x + 2*x*y + y + y*y + 1362)

fnbrs (x,y) = filter open [(x+1,y),(x-1,y),(x,y+1),(x,y-1)]

locations = iterate (nub . concatMap fnbrs) [(1,1)] :: [[(Int,Int)]]

main = print (length $ takeWhile (all (/= (31,39))) locations,
              length $ nub $ concat $ take 51 locations)
