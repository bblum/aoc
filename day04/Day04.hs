contained :: [Int] -> Bool
contained [a,b,c,d] = a <= c && d <= b || c <= a && b <= d

overlaps [a,b,c,d] = a <= c && c <= b || c <= a && a <= d

main = do input <- map (map read . words) <$> lines <$> readFile "input.txt"
          print $ length $ filter contained input
          print $ length $ filter overlaps input
