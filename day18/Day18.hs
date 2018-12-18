import Data.List

tick grid = [ [ rule (y,x) c | (c,x) <- zip row [0..] ] | (row,y) <- zip grid [0..] ]
    where rule yx '.' = if fnbrs yx '|' > 2 then '|' else '.'
          rule yx '|' = if fnbrs yx '#' > 2 then '#' else '|'
          rule yx '#' = if fnbrs yx '#' > 0 && fnbrs yx '|' > 0 then '#' else '.'
          fnbrs (y,x) c = sum [ 1 | w <- intersect [y-1..y+1] [0..length grid - 1],
                                    z <- intersect [x-1..x+1] [0..length (head grid) - 1],
                                    (w,z) /= (y,x), c == grid !! w !! z ]

score grid = count '#' * count '|' where count x = sum [ 1 | r <- grid, c <- r, c == x ]

main = do input <- lines <$> readFile "input.txt"
          print $ score $ iterate tick input !! 10
          print $ score $ iterate tick input !! 2400
