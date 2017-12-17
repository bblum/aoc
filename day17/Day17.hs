ins (buf,i) val = (take j buf ++ [val] ++ drop j buf, j)
    where j = 1 + mod (i + 377) (length buf)

main = print $ uncurry (!!) $ flip mod 2017 <$> (+1) <$> foldl ins ([0],0) [1..2017]
