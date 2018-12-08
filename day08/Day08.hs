part1 (result,(n:m:rest)) = meta m $ children n (result,rest)
    where children 0 x = x
          children n x = children (n-1) $ part1 x
          meta m (result,rest) = (result + sum (take m rest), drop m rest)
part1 x = x

part2 (n:m:rest) = meta m $ children n rest
    where children 0 rest = ([],rest)
          children n rest = (result:cs, rest')
              where (result, (cs,rest')) = children (n-1) <$> part2 rest
          meta m ([], rest) = (sum $ take m rest, drop m rest)
          meta m (cs, rest) = (sum $ map (value cs) $ take m rest, drop m rest)
          value cs i = sum $ take 1 $ drop (i-1) cs
part2 _ = (0,[])

main = do input <- map read <$> words <$> head <$> lines <$> readFile "input.txt"
          print $ fst $ part1 (0,input)
          print $ fst $ part2 input
