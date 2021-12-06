import Data.List.Split

count f = length . filter f

parse [ij, [c, ':'], password] = (map read $ splitOn "-" ij, c, password)

legal1 ([i,j], c, password) = n >= i && n <= j
    where n = count (== c) password

legal2 (ij, c, password) = count id (map (\x -> password !! (x-1) == c) ij) == 1

main = do input <- map (parse . words) <$> lines <$> readFile "input.txt"
          print $ count legal1 input
          print $ count legal2 input
