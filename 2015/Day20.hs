import Data.Set (toList)
import Data.List
import Data.Maybe
import Control.Arrow
import Math.NumberTheory.ArithmeticFunctions

-- elf n = cycle $ 10*n:replicate (n-1) 0
-- house n = sum $ map (\e -> elf e !! (n-e)) [1..n]

part1 = ((10::Int)*) . sigma 1
part2 = ((11::Int)*) . sum . filter (\d -> d * 50 >= n) . toList . divisors n

solve house = (1+) $ fromJust $ findIndex (>=36000000) $ map house [1..]

main = print (solve part1, solve part2)
