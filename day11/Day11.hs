import qualified Data.Map as M
import Data.Maybe

coordify input = M.fromList $ concat $ map repair $ zip [0..] $ map (zip [0..]) input
    where repair (y,xs) = map (\(x,c) -> ((y,x),c)) xs

count f = length . filter f
occupied = count (== '#')

nbrs (y,x) = [(y2,x2) | y2 <- [y-1..y+1], x2 <- [x-1..x+1], (y2,x2) /= (y,x)]

step f n input = M.mapWithKey stepcell input
    where stepcell yx '.' = '.'
          stepcell yx 'L' = if f input yx == 0 then '#' else 'L'
          stepcell yx '#' = if f input yx >= n then 'L' else '#'

adjacent input yx = occupied $ mapMaybe (flip M.lookup input) $ nbrs yx

visible input yx = count (look yx) $ nbrs (0,0)
    where look (y,x) (dy,dx) = case M.lookup newyx input of
              Just '#' -> True
              Just '.' -> look newyx (dy,dx)
              _ -> False
              where newyx = (y+dy,x+dx)

simulate f n x = let x2 = step f n x in if x2 == x then x else simulate f n x2

main = do input <- coordify <$> lines <$> readFile "input.txt"
          print $ occupied $ M.elems $ simulate adjacent 4 input
          print $ occupied $ M.elems $ simulate visible 5 input
