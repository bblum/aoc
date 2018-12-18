import Data.List

update grid (y,x) = rule $ grid !! y !! x
    where rule '.' = if fnbrs '|' > 2 then '|' else '.'
          rule '|' = if fnbrs '#' > 2 then '#' else '|'
          rule '#' = if fnbrs '#' > 0 && fnbrs '|' > 0 then '#' else '.'
          fnbrs c = sum [ 1 | w <- intersect [y-1..y+1] [0..length grid - 1],
                              z <- intersect [x-1..x+1] [0..length (head grid) - 1],
                              (w,z) /= (y,x) && (grid !! w !! z) == c ]

minute grid = map (map (update grid)) $ coords grid

coords grid = map (flip map [0..length (head grid) - 1] . (,)) [0..length grid - 1]

score grid = count '#' * count '|' where count x = sum [ 1 | r <- grid, c <- r, c == x ]

main = do input <- lines <$> readFile "input.txt"
          print $ score $ iterate minute input !! 10
          print $ score $ iterate minute input !! 2400
