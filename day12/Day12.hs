import qualified Data.Set as S
import Data.List
import Data.Maybe

nbrs l (_,g0) = (g0 == g, g)
    where g = S.foldr (\n g -> foldr S.insert g $ fromJust $ lookup n l) g0 g0

group0 l = snd $ until fst (nbrs l) (False, (S.singleton $ fst $ head l))

ungroup l = S.foldr (\n -> filter ((/= n) . fst)) l $ group0 l

nobe (n:_:rest) = (read n :: Int, map (read . filter (/= ',')) rest)

main = do input <- map (nobe . words) <$> lines <$> readFile "input.txt"
          print $ length $ group0 input
          print $ length $ takeWhile (not . null) $ iterate ungroup input
