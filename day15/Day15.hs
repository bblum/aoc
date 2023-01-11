import Data.List
import Data.Maybe

solverow n [sx,sy,bx,by] = if rowwidth < 0 then Nothing else Just ans
    where dist = abs (bx-sx) + abs (by-sy)
          rowwidth = dist - abs (sy-n)
          ans = (sx-rowwidth, sx+rowwidth)

part1 input = maximum maxs - minimum mins -- cheating but works
    where (mins,maxs) = unzip $ mapMaybe (solverow 2000000) input

-- dont need to bother bounding 0<x<4M
coalesce ((a,b):(c,d):rest) | b >= c = coalesce $ (a, max b d):rest
coalesce (x:rest) = x:coalesce rest
coalesce [] = []

findgap input n = if length noob > 1 then Just (n,noob) else Nothing
    where noob = coalesce $ sortOn fst $ mapMaybe (solverow n) input

part2 [(y,[(_,x),_])] = (x+1) * 4000000 + y

main = do input <- map (map read . words) <$> lines <$> readFile "input.txt"
          print $ part1 input
          print $ part2 $ mapMaybe (findgap input) [0..4000000]
