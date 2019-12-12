import Data.List
import Data.Maybe

moons = map (:[[0,0,0]]) [[15,-2,-6], [-5,-4,-11], [0,-6,0], [5,9,6]]

gravity [pos,vel] [from,_] = [pos, zipWith3 pull from pos vel]
    where pull from to v = if from < to then v-1 else if from > to then v+1 else v

step moons = map velocity [ foldl gravity moon moons | moon <- moons ]
    where velocity [pos,vel] = [zipWith (+) pos vel, vel]

solve axis = fst $ fromJust $ find (seen axis) $ zip [1..] $ tail $ iterate step moons
    where seen axis (_,newmoons) = map (map (!! axis)) moons == map (map (!! axis)) newmoons

main = do print $ sum $ map (product . map (sum . map abs)) $ iterate step moons !! 1000
          print $ foldl lcm 1 $ map solve [0..2]
