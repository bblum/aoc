main = interact $ (++"\n") . show . length . takeWhile (>= 0) . scanl (\fl ch -> if ch == '(' then fl+1 else fl-1) 0
