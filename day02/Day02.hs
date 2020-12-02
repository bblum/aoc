import Data.List.Split

parse [ij, [c, ':'], password] = (map read $ splitOn "-" ij, c, password)

legal1 ([i,j], c, password) = count >= i && count <= j
    where count = length $ filter (== c) password

legal2 (ij, c, password) = length (filter id (map (\x -> password !! (x-1) == c) ij)) == 1

main = do input <- map (parse . words) <$> lines <$> readFile "input.txt"
          print $ length $ filter legal1 input
          print $ length $ filter legal2 input
