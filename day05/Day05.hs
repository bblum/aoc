import qualified Data.Map as M

start = ["VRHBGDW","FRCGNJ","JNDHFSL","VSDJ","VNWQRDHS","MCHGP","CHZLGBJF","RJS","MVNBRSGL"]

step f state [n,i,j] = M.insert i left $ M.adjust (f taken ++) j state
    where (taken,left) = splitAt n $ state M.! i

solve f = map (head . snd) . M.toList . foldl (step f) (M.fromList $ zip [1..] start)

main = do steps <- map (map read . words) <$> lines <$> readFile "input.txt"
          print $ solve reverse steps
          print $ solve id steps
