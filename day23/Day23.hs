import qualified Data.Map as M

input = [5,8,3,9,7,6,2,4,1]

crab (c0, cups) = (cups2 M.! c0, cups2)
    where [c1,c2,c3,c4] = map (cups M.!) [c0,c1,c2,c3]
          dest c0 = if elem d [c1,c2,c3] then dest d else d
              where d = mod (c0-2) (M.size cups) + 1
          cups2 = foldr (uncurry M.insert) cups [(c0,c4),(dest c0,c1),(c3,cups M.! dest c0)]

solve p n k = take p $ tail $ iterate (snd (iterate crab (head l, M.fromList $ zip (last l:l) l) !! k) M.!) 1
    where l = input ++ [10..n]

main = do putStrLn $ concatMap show $ solve 8 9 100
          print $ product $ solve 2 1000000 10000000
