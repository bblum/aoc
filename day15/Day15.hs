import qualified Data.Map as M

input = [1,0,18,10,19,6]

step (n, (t, seen)) = (maybe 0 (t-) (M.lookup n seen), (t+1, M.insert n t seen))

solve n = fst $ iterate step (i, (length rest, M.fromList $ zip (reverse rest) [0..])) !! (n - length input)
    where i:rest = reverse input

main = do print $ solve 2020
          print $ solve 30000000
