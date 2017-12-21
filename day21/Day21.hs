import Data.List
import Data.List.Split
import Data.Maybe

glider = [".#.","..#","###"]

rotations noob = noobs ++ map transpose noobs
    where noobs = [noob, reverse noob, map reverse noob, map reverse $ reverse noob]

findrule rules noob = head $ catMaybes $ map (flip lookup rules) $ rotations noob

breakline2 [] [] = []
breakline2 (a1:a2:a) (b1:b2:b) = [[a1,a2],[b1,b2]] : breakline2 a b
break2 [] = []
break2 (a:b:rest) = breakline2 a b : break2 rest

breakline3 [] [] [] = []
breakline3 (a1:a2:a3:a) (b1:b2:b3:b) (c1:c2:c3:c) = [[a1,a2,a3],[b1,b2,b3],[c1,c2,c3]] : breakline3 a b c
break3 [] = []
break3 (a:b:c:rest) = breakline3 a b c : break3 rest

assemblerow3 noobs = map (flip concatMap noobs . flip (!!)) [0..2]
assemble3 grid = concatMap assemblerow3 grid

assemblerow4 noobs = map (flip concatMap noobs . flip (!!)) [0..3]
assemble4 grid = concatMap assemblerow4 grid

step rules noob =
    if length (head noob) `mod` 2 == 0 then
        assemble3 $ map (map $ findrule rules) $ break2 noob
    else
        assemble4 $ map (map $ findrule rules) $ break3 noob

solve rules n = sum $ map (length . filter (=='#')) $ iterate (step rules) glider !! n

main = do input <- map ((\[x,y]->(x,y)) . map (splitOn "/") . words) <$> lines <$> readFile "input.txt"
          print $ solve input 5
          print $ solve input 18
