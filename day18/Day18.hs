import Data.List

update grid (y,x) = update' $ grid !! y !! x
    where update' '.' = if fnbrs '|' > 2 then '|' else '.'
          update' '|' = if fnbrs '#' > 2 then '#' else '|'
          update' '#' = if fnbrs '#' > 0 && fnbrs '|' > 0 then '#' else '.'
          fnbrs c = length $ filter (==c) $ map (uncurry $ (!!) . (grid !!)) $
                        [ (w,z) | z <- intersect [x-1..x+1] [0..length (head grid) - 1],
                                  w <- intersect [y-1..y+1] [0..length grid - 1] ] \\ [(y,x)]

updateall grid = map (map (update grid)) $ coords grid

coords grid = map (flip map [0..length (head grid) - 1] . (,)) [0..length grid - 1]

score grid = count '#' * count '|' where count c = length $ filter (==c) $ concat grid

main = do input <- lines <$> readFile "input.txt"
          print $ score $ iterate updateall input !! 10
          print $ score $ iterate updateall input !! 2400
