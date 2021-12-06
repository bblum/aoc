import Data.List.Split

solve input (dx,dy) = length $ filter tree $ zip [0..] $ chunksOf dy input
    where tree (y, line:_) = line !! (y * dx `mod` length line) == '#'

main = do input <- lines <$> readFile "input.txt"
          print $ solve input (3,1)
          print $ product $ map (solve input) [(1,1),(3,1),(5,1),(7,1),(1,2)]
