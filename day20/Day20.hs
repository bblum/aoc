import Data.List
import Data.Ord

data Particle = P { pos :: (Int, Int, Int), vel :: (Int, Int, Int), acc :: (Int, Int, Int), name :: Int }

parse (n,["p",px,py,pz,"v",vx,vy,vz,"a",ax,ay,az]) = P (read px, read py, read pz) (read vx, read vy, read vz) (read ax, read ay, read az) n

add3 (a,b,c) (d,e,f) = (a+d,b+e,c+f)

tick p = p { pos = add3 v $ pos p, vel = v } where v = add3 (vel p) (acc p)

dist p = abs px + abs py + abs pz where (px,py,pz) = pos p

collide ps = filter (\p -> (length $ filter (== pos p) $ map pos ps) == 1) ps

main = do ps <- map parse <$> zip [0..] <$> map words <$> lines <$> readFile "input.txt"
          print $ name $ minimumBy (comparing dist) $ iterate (map tick) ps !! 1000
          print $ length $ iterate (map tick . collide) ps !! 1000
