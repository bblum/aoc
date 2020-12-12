parse (c:rest) = (c, read rest)

ship ([y,x],facing) ('N', noob) = ([y-noob,x],facing)
ship ([y,x],facing) ('E', noob) = ([y,x+noob],facing)
ship ([y,x],facing) ('S', noob) = ([y+noob,x],facing)
ship ([y,x],facing) ('W', noob) = ([y,x-noob],facing)
ship (yx,facing) ('L', noob) = (yx, mod (facing - div noob 90) 4)
ship (yx,facing) ('R', noob) = (yx, mod (facing + div noob 90) 4)
ship (yx,facing) ('F', noob) = ship (yx,facing) (dir facing, noob)
    where dir 0 = 'N'
          dir 1 = 'E'
          dir 2 = 'S'
          dir 3 = 'W'

ship2 (yx, (wayy,wayx)) ('N', noob) = (yx, (wayy-noob, wayx))
ship2 (yx, (wayy,wayx)) ('E', noob) = (yx, (wayy, wayx+noob))
ship2 (yx, (wayy,wayx)) ('S', noob) = (yx, (wayy+noob, wayx))
ship2 (yx, (wayy,wayx)) ('W', noob) = (yx, (wayy, wayx-noob))
ship2 ([y,x], (wayy,wayx)) ('F', noob) = ([y+dy,x+dx], (wayy+dy,wayx+dx))
    where (dy,dx) = (noob * (wayy-y), noob * (wayx-x))
ship2 ([y,x], (wayy,wayx)) (lr, noob) = ([y,x], turn lr noob)
    where (dy,dx) = (y-wayy, x-wayx)
          turn 'L' 90 = (y+dx, x-dy)
          turn 'L' 180 = (y+dy, x+dx)
          turn 'L' 270 = (y-dx, x+dy)
          turn 'R' noob = turn 'L' $ 360-noob

solve f start = sum . map abs . fst . foldl f start

main = do input <- map parse <$> lines <$> readFile "input.txt"
          print $ solve ship ([0,0],1) input
          print $ solve ship2 ([0,0],(-1,10)) input

