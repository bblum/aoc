import Data.List

marker n i input | take n input == nub (take n input) = i
marker n i input = marker n (i+1) $ tail input

main = do input <- readFile "input.txt"
          print $ marker 4 4 input
          print $ marker 14 14 input
