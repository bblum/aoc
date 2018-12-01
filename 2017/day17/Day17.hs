part1 (buf,i) val = (take j buf ++ [val] ++ drop j buf, j)
    where j = 1 + mod (i + 377) (length buf)

part2 (i,result) val = seq result (j, if j == 1 then val + 1 else result)
    where j = 1 + mod (i + 377) (val + 1)

main = do print $ uncurry (!!) $ flip mod 2017 <$> (+1) <$> foldl part1 ([0],0) [1..2017]
          print $ snd $ foldl part2 (0,0) [0..50000000]
