next n x = mod (x * n) 2147483647
gen m n = iterate (until (\x -> mod x m == 0) (next n) . next n)
solve z ma mb = length $ filter (\(a,b) -> mod a 65536 == mod b 65536) $ take z $ zip (gen ma 16807 679) (gen mb 48271 771)
main = print (solve 40000000 1 1, solve 5000000 4 8)
