import Data.List

update input = [ [ rule (y,x) c | (c,x) <- zip row [0..] ] | (row,y) <- zip input [0..] ]
    where rule yx '.' = if adj yx '|' > 2 then '|' else '.'
          rule yx '|' = if adj yx '#' > 2 then '#' else '|'
          rule yx '#' = if adj yx '#' > 0 && adj yx '|' > 0 then '#' else '.'
          adj (y,x) c = sum [ 1 | w <- intersect [y-1..y+1] [0..length input - 1],
                                  z <- intersect [x-1..x+1] [0..length (head input) - 1],
                                  (w,z) /= (y,x), c == input !! w !! z ]

score input = count '#' * count '|' where count x = sum [ 1 | r <- input, c <- r, c == x ]

main = do input <- lines <$> readFile "input.txt"
          print $ score $ iterate update input !! 10
          print $ score $ iterate update input !! 2400
