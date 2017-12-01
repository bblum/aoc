import Data.List
import Data.List.Split

main = interact $ (++"\n") . show . length . filter (\[x,y,z] -> x + y > z) . map (sort . map read) . concat . map transpose . chunksOf 3 . map words . lines
