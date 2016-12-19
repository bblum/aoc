steal l = let newl = filter (/= (l !! div (length l) 2)) l -- kill an elf
          in drop 1 newl ++ take 1 newl -- rotate killer to the back

solve n = head $ until ((==1) . length) steal [1..n]

main = print $ zip [1..] $ map solve [1..100]
