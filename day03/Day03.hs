segmence latency (x,y) output [] = output
segmence latency (x,y) output ((dir:asdf):rest) = segmence (latency+dist) (newx,newy) (seg:output) (rest) where
    dist = read asdf
    move 'R' dist = (x+dist, y)
    move 'L' dist = (x-dist, y)
    move 'U' dist = (x, y-dist)
    move 'D' dist = (x, y+dist)
    (newx,newy) = move dir dist
    seg = ((x,y),(newx,newy), latency)

between a b c = a >= b && a <= c || a <= b && a >= c

crossings ((a,b),(c,d),latency1) ((e,f),(g,h),latency2) | a == c && f == h =
    if between f b d && between a e g then
        [((a,f), latency1 + abs (f - b) + latency2 + abs (a - e))]
    else []
crossings ((a,b),(c,d),latency1) ((e,f),(g,h),latency2) | b == d && e == g =
    crossings ((e,f),(g,h),latency2) ((a,b),(c,d),latency1)
crossings _ _ = []

manhattan ((x,y),_) = abs x + abs y

main = do [wire1, wire2] <- map words <$> lines <$> readFile "input.txt"
          let segs1 = segmence 0 (0,0) [] wire1
          let segs2 = segmence 0 (0,0) [] wire2
          let poince = concat [crossings seg1 seg2 | seg1 <- segs1, seg2 <- segs2]
          print $ minimum $ map manhattan poince
          print $ minimum $ map snd poince
