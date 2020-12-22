import qualified Data.Set as S
import Data.List.Split

score l = sum $ zipWith (*) [1..] $ reverse l

play seen p1s p2s | S.member (p1s,p2s) seen = (True, score p1s)
play seen p1 [] = (True, score p1)
play seen [] p2 = (False, score p2)
-- play seen (p1:p1s) (p2:p2s) = if p1 > p2 then play newseen (p1s ++ [p1,p2]) p2s else play newseen p1s (p2s ++ [p2,p1])
play seen (p1:p1s) (p2:p2s) =
    if length p1s >= p1 && length p2s >= p2 then
        let (p1_wins, subscore) = play S.empty (take p1 p1s) (take p2 p2s)
        in if p1_wins then
               play newseen (p1s ++ [p1,p2]) p2s
           else
               play newseen p1s (p2s ++ [p2,p1])
    else
        if p1 > p2 then play newseen (p1s ++ [p1,p2]) p2s else play newseen p1s (p2s ++ [p2,p1])
    where newseen = S.insert (p1:p1s,p2:p2s) seen

main = do [p1,p2] <- splitOn [""] <$> lines <$> readFile "input.txt"
          print $ play S.empty (map read p1) (map read p2)
