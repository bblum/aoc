import Data.List.Extra
import Data.Maybe

nums = [17,2,33,86,38,41,4,34,91,61,11,81,3,59,29,71,26,44,54,89,46,9,85,62,23,76,45,24,78,14,58,48,57,40,21,49,7,99,8,56,50,19,53,55,10,94,75,68,6,83,84,88,52,80,73,74,79,36,70,28,37,0,42,98,96,92,27,90,47,20,5,77,69,93,31,30,95,25,63,65,51,72,60,16,12,64,18,13,1,35,15,66,67,43,22,87,97,32,39,82]

call (called, n:next) = (n:called, next)

wins calleds board = any winrow board || any winrow (transpose board)
    where winrow = all $ flip elem calleds

score calleds = (head calleds *) . sum . filter (not . flip elem calleds) . concat 

-- ugh this is really not a haskell friendly problem
play boards = (solve calleds1 $ wins calleds1, solve calleds2 $ not . wins (fst state2))
    where gameover1 f = flip f boards . wins . fst
          gameover2 state = (==1) $ length $ filter (not . (wins $ fst state)) boards
          calleds1 = fst $ until (gameover1 any) call ([],nums)
          state2 = until gameover2 call ([], nums)
          calleds2 = fst $ until (gameover1 all) call state2
          solve calleds f = score calleds $ fromJust $ find f boards

main = do input <- splitOn [[]] <$> map (map read . words) <$> lines <$> readFile "input.txt"
          print $ play input
