import Data.List

sleep fhr fmin whr wmin =
    -- you don't even need to make it this complicated lmao, hr is always 00
    -- i didn't read the directions
    if whr < fhr then sleep fhr fmin (whr + 24) wmin else
    if wmin < fmin then sleep fhr fmin (whr - 1) (wmin + 60)
    else ((whr - fhr) * 60) + (wmin - fmin)

sleep' a b c d = sleep (read a) (read b) (read c) (read d)

guardify output ([_,_,_, "Guard",('#':num),_,_]:rest) = guardify ([read num]:output) rest
guardify (g:output) ([_,fhr,fmin,"falls",_]:[_,whr,wmin,"wakes",_]:rest) =
    guardify ( ((sleep' fhr fmin whr wmin):g):output) rest
guardify output [] = output

sleepify gs = (head $ head gs, sum $ concat $ map tail gs)

guardify' input = groupBy (\x y -> head x == head y) $ sortBy (\x y -> compare (head x) $head y) $ map reverse $ guardify [] input

-- part 2

guardify2 :: [[Int]] -> [[String]] -> [[Int]]
guardify2 output ([_,_,_, "Guard",('#':num),_,_]:rest) = guardify2 ([read num]:output) rest
guardify2 (g:output) ([_,fhr,fmin,"falls",_]:[_,whr,wmin,"wakes",_]:rest) =
    guardify2 ( (read wmin:read fmin:g):output) rest
guardify2 output [] = output

guardify2' input = groupBy (\x y -> head x == head y) $ sortBy (\x y -> compare (head x) $head y) $ map reverse $ guardify2 [] input

asleeponmin :: Int -> [Int] -> Int
asleeponmin m [] = 0
asleeponmin m (f:w:rest) = if m >= f && m < w then 1 else asleeponmin m rest
minutecount days m = sum $ map (asleeponmin m) days

sleepcount gs = (head $ head gs, map (minutecount $ map tail gs) [0..59])

foo (gid,times) = maximum times

compareBy f x y = compare (f x) (f y)

main = do input <- map words <$> lines <$> readFile "input.txt"
          -- like a big idiot, i then combed the input by hand for this guard id
          -- and charted the guard's sleep times and found the max minute by hand
          print $ maximumBy (compareBy snd) $ map sleepify $ guardify' input
          -- found the index of the noob that's max, by hand
          print $ maximumBy (compareBy foo) $ map sleepcount $ guardify2' input
