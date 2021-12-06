{-# LANGUAGE TupleSections #-}
import qualified Data.Set as S
import Control.Arrow

range :: Int -> Int -> [Int]
range a b | a < b = [a..b]
range a b = reverse $ range b a

poince _ [x1,y1,x2,y2] | x1 == x2 = map (x1,) $ range y1 y2
poince _ [x1,y1,x2,y2] | y1 == y2 = map (,y1) $ range x1 x2
poince 1 _ = []
poince 2 [x1,y1,x2,y2] = zip (range x1 x2) (range y1 y2)

visit (onces,mores) xy = if S.member xy onces then (onces, S.insert xy mores) else (S.insert xy onces, mores)

solve p input = S.size $ snd $ foldl visit (S.empty, S.empty) $ concatMap (poince p) input

main = print =<< (solve 1 &&& solve 2) <$> map (map read . words) <$> lines <$> readFile "input.txt"
