fuel x = if f == 0 then 0 else f + fuel f
    where f = max 0 $ (div x 3) - 2

main = do input <- map read <$> lines <$> readFile "input.txt"
          print $ sum $ map (\x -> (div x 3) - 2) $ input
          print $ sum $ map fuel input
