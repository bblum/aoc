import qualified Data.Set as S

move state [d,n] = iterate (step d) state !! read n

step d (((hy,hx):rest),visited) = (h2:rest2, S.insert (last rest2) visited)
    where dir "L" = (hy,hx-1)
          dir "R" = (hy,hx+1)
          dir "D" = (hy-1,hx)
          dir "U" = (hy+1,hx)
          h2 = dir d
          rest2 = follow h2 rest
          follow h2 [] = []
          follow (hy2,hx2) ((ty,tx):rest) = t2:follow t2 rest
              where t2 = follow1 (hy2-ty) (hx2-tx)
                    -- still touching, dont move tx
                    follow1 dy dx | abs dy < 2 && abs dx < 2 = (ty,tx)
                    -- move however much is appropriate in both dimensions
                    follow1 dy dx = (ty + v !! (dy+2), tx + v !! (dx+2))
                    v = [-1,-1,0,1,1]

solve n = S.size . snd . foldl move (replicate n (0,0), S.singleton (0,0))

main = do input <- map words <$> lines <$> readFile "input.txt"
          print $ solve 2 input
          print $ solve 10 input
