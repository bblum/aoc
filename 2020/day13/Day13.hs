import Data.List
import Data.Ord

start = 1013728
x = 0
input = [23,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,733,x,x,x,x,x,x,x,x,x,x,x,x,13,17,x,x,x,x,19,x,x,x,x,x,x,x,x,x,29,x,449,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37]

sample1 = [7,13,x,x,59,x,31,19]
sample2 = [7,13,3,4,59,18,31,19]

part1 input = (time - start) * bus
    where buses = filter (>0) input
          bustime b = (1 + div start b) * b
          (bus,time) = minimumBy (comparing snd) $ zip buses $ map bustime buses

part2 input = fst $ foldl crt (0,1) $ filter ((>0) . fst) $ zip input [0..]
    where crt (base,step) (bus,offset) = (head $ filter ok [base,base+step..], lcm bus step)
              where ok t = mod t bus == mod (bus - offset) bus

main = do print $ part1 input
          print $ part2 sample1 -- 1068781
          print $ part2 sample2 -- 1068781
          print $ part2 input
