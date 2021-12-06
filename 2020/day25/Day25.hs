import Math.NumberTheory.Powers.Modular

a = 12232269
b = 19452773
m = 20201227

main = print $ powMod b (head $ filter (\loop -> powMod 7 loop m == a) [0..]) m
