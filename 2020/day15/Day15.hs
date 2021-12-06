import qualified Data.Map as M

input = [1,0,18,10,19,6]

step (n, (t, seen)) = (maybe 0 (t-) (M.lookup n seen), (t+1, M.insert n t seen))

solve n = fst $ iterate step (0, (length input, M.fromList $ zip input [0..])) !! (n - length input - 1)

main = do print $ solve 2020
          print $ solve 30000000
