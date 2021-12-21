diff = (abs .) . subtract
triangle x y = div (d*(d+1)) 2 where d = diff x y

main = do crabs <- map read <$> words <$> head <$> lines <$> readFile "input.txt"
          let solve f = minimum $ map (sum . flip map crabs . f) [minimum crabs..maximum crabs]
          print $ solve diff
          print $ solve triangle
