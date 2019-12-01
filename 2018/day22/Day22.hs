import Data.Maybe

depth = 9465
ty = 704
tx = 13

ero = (fromJust . flip lookup e)
    where e = [ ((y,n-y), mod (geo y (n-y) + depth) 20183) | n <- [0..], y <- [0..n] ]
          geo 0 x = x * 16807
          geo y 0 = y * 48271
          geo y x | y == ty && x == tx = 0
          geo y x = ero (y,x-1) * ero (y-1,x)

main = print $ sum [ mod (ero (y,x)) 3 | y <- [0..ty], x <- [0..tx] ]
