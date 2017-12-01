fwd (x,y,0) dist = (x, y + dist, 0)
fwd (x,y,1) dist = (x + dist, y, 1)
fwd (x,y,2) dist = (x, y - dist, 2)
fwd (x,y,3) dist = (x - dist, y, 3)

walk (x,y,heading) ('L':dist) = fwd (x, y, (heading + 3) `mod` 4) $ read dist
walk (x,y,heading) ('R':dist) = fwd (x, y, (heading + 1) `mod` 4) $ read dist

foo lines = let (x,y,_) = foldl walk (0,0,0) lines in abs x + abs y

main = interact $ (++"\n") . show . foo . lines
