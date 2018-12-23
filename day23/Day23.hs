parse ["pos",x,y,z,"r",r] = ((read x, read y, read z), read r)

manh (x,y,z) (a,b,c) = abs (x - a) + abs (y - b) + abs (z - c)

range (xyz,r) (abc,s) = manh abc xyz <= r

-- output some z3 lisp
shownum n = if n < 0 then "(- 0" ++ show (0-n) ++ ")" else show n
output ((a,b,c),r) = "(assert-soft (>= " ++ shownum r ++" (+ (manh x " ++ shownum a ++ ") (manh y " ++ shownum b ++ ") (manh z " ++ shownum c ++"))))"

main = do input <- map parse <$> map words <$> lines <$> readFile "input.txt"
          -- part1
          -- let (maxbot,maxi) = maximumBy (comparing $ snd . fst) $ zip input [0..]
          -- print $ length $ filter (range maxbot) input
          mapM putStrLn $ map output input
