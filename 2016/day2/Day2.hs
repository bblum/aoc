-- Part 1
-- board = ["xxxxx","x123x","x456x","x789x","xxxxx"]
-- startPos = (2,2)

-- Part 2
board = ["xxxxxxx","xxx1xxx","xx234xx","x56789x","xxABCxx","xxxDxxx","xxxxxxx"]
startPos = (1,3)

valueAt (x,y) = board !! y !! x

step 'L' (x,y) = (x-1, y)
step 'R' (x,y) = (x+1, y)
step 'U' (x,y) = (x, y-1)
step 'D' (x,y) = (x, y+1)

bar xy dir = if valueAt (step dir xy) == 'x' then xy else step dir xy

foo (xy0, result) input = let xy = foldl bar xy0 input in (xy, (valueAt xy):result)

main = interact $ (++"\n") . show . reverse . snd . foldl foo (startPos,[]) . lines
