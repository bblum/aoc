{-# LANGUAGE TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.List.Extra
import Data.Tuple
import Data.Tuple.Extra

start = (0,((8,0),(5,0)))

board i = (10:[1..9]) !! mod i 10

yourturn turn n ps = (n+1,) <$> if mod n 2 == 0 then turn ps else swap <$> turn (swap ps)

--

deterministic n ((p,score),other) = [((p2, score + p2), other)]
    where p2 = board $ p + sum (chunksOf 3 (cycle [1..100]) !! n)

part1 (n,ps) = head $ yourturn (deterministic n) n ps

gameover (_,((_,s1),(_,s2))) = s1 >= 1000 || s2 >= 1000

solve (s@(n,((_,s1),(_,s2)))) = 3 * n * min s1 s2

---

dirac ((p,score),other) = [ ((p2, score + p2), other) | x <- [1..3], y <- [1..3], z <- [1..3], let p2 = board $ x+y+z+p ]

eval (m,out) (n,((_,s1),(_,s2))) | s1 >= 21 = (m,(1,0):out)
eval (m,out) (n,((_,s1),(_,s2))) | s2 >= 21 = (m,(0,1):out)
eval (m,out) (n,ps) = maybe (M.insert (n,ps) ans m2, ans:out) ((m,) . (:out)) (M.lookup (n,ps) m)
    where (m2,ans) = part2 m $ yourturn dirac n ps

part2 m states = both sum <$> unzip <$> foldl eval (m,[]) states

main = do print $ solve $ until gameover part1 start
          print $ uncurry max $ snd $ part2 M.empty [start]
