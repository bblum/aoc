import Data.List
import Data.Ord

start = 1013728
input = [23,x,x,x,x,x,x,x,x,x,x,x,x,41,x,x,x,x,x,x,x,x,x,733,x,x,x,x,x,x,x,x,x,x,x,x,13,17,x,x,x,x,19,x,x,x,x,x,x,x,x,x,29,x,449,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,x,37] where x = 0

part1 = (time - start) * bus
    where buses = filter (>0) input
          bustime b = (1 + div start b) * b
          (bus,time) = minimumBy (comparing snd) $ zip buses $ map bustime buses

part2 = fst $ foldl asdf (0,1) schedule
    where schedule = filter ((>0) . fst) $ zip input [0..]
          asdf (base,step) (bus,offset) = (newbase, next - newbase)
              where works t = mod t bus == mod (bus - offset) bus
                    (newbase:next:_) = filter works [base,base+step..]

main = do print part1
          print part2
