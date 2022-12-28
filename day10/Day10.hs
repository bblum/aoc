import Data.List.Split

exec (c,x) [] = []
exec (c,x) (["noop"]:rest) = [r] ++ (exec r rest) where r = (c+1,x)
exec (c,x) (["addx",i]:rest) = [(c+1,x),r] ++ (exec r rest) where r = (c+2, x + read i)

part1 c = mod (c-20) 40 == 0

part2 (c,x) = if elem x [c2-1..c2+1] then '#' else ' ' where c2 = mod c 40

main = do input <- map words <$> lines <$> readFile "input.txt"
          let output = exec (0,1) input
          print $ sum $ map (uncurry (*)) $ filter (part1 . fst) output
          mapM print $ chunksOf 40 $ map part2 $ [(0,1)] ++ output
