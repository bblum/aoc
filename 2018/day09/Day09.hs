import Data.Map (fromList,adjust,elems)

ins n (p,a:b:q) = (b:a:p, n:q)
ins n (p,q) = ins n ([], q ++ reverse p)

del k n (m,(a:b:c:d:e:f:g:p,q)) = (adjust (+(g+n)) (mod n k) m, (p, f:e:d:c:b:a:q))
del k n (m,(p,q)) = del k n (m, (p ++ reverse q, []))

marble k state n = if mod n 23 == 0 then del k n state else ins n <$> state

solve k n = maximum $ elems $ fst $ foldl (marble k) (m,([0,1],[])) [2..n]
    where m = fromList $ zip [0..k-1] $ repeat 0

main = print (solve 418 71339, solve 418 7133900)
