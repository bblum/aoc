-- this takes a good while to run - i didn't time it, but like an hour? - but it's elegant and i liked this problem

import Data.List
import Data.List.Split
import Debug.Trace

-- b is the 'block' and h is the 'hammer' we are shattering it with
-- returns a list of the subtracted region (empty or singleton),
-- and a list of up to 2 unaffected regions
shatter1d bx0 bx1 hx0 hx1 =
    case sort [bx0,bx1,hx0,hx1] of
        l | l == [bx0,bx1,hx0,hx1] && bx1 < hx0              -> ([],         [(bx0,bx1)]) -- missed!
        l | l == [hx0,hx1,bx0,bx1] && hx1 < bx0              -> ([],         [(bx0,bx1)]) -- missed!
        l | l == [bx0,hx0,bx1,hx1] && bx0 < hx0              -> ([(hx0,bx1)],[(bx0,hx0-1)]) -- hit, right side
        l | l == [hx0,bx0,hx1,bx1] && hx1 < bx1              -> ([(bx0,hx1)],[(hx1+1,bx1)]) -- hit, left side
        l | l == [bx0,hx0,hx1,bx1] && bx0 < hx0 && hx1 < bx1 -> ([(hx0,hx1)],[(bx0,hx0-1),(hx1+1,bx1)]) -- middle split
        l | l == [hx0,bx0,bx1,hx1]                           -> ([(bx0,bx1)],[]) -- entire range of block affected
        _ -> error $ "ranges flipped: " ++ show [bx0,bx1,hx0,hx1]

shatters blocks [hx0,hx1,hy0,hy1,hz0,hz1] = concatMap shatter blocks
    where shatter [bx0,bx1,by0,by1,bz0,bz1] = xblocks ++ yblocks ++ zblocks
              where (xhits,xmisseds) = shatter1d bx0 bx1 hx0 hx1
                    (yhits,ymisseds) = shatter1d by0 by1 hy0 hy1
                    (_,    zmisseds) = shatter1d bz0 bz1 hz0 hz1
                    xblocks = [ [x0,x1,by0,by1,bz0,bz1] | (x0,x1) <- xmisseds ]
                    yblocks = [ [x0,x1,y0,y1,bz0,bz1] | (x0,x1) <- xhits, (y0,y1) <- ymisseds ]
                    zblocks = [ [x0,x1,y0,y1,z0,z1] | (x0,x1) <- xhits, (y0,y1) <- yhits, (z0,z1) <- zmisseds ]

-- subtract regions from states
switch states (n,("off":block)) = traceShow (n,"off, #blocks:",length states) $ shatters states $ map read block
-- subtract regions from block and add to states
switch states (n,("on":block)) = traceShow (n,"on, #blocks:",length states2,"pruned: ",length states-length states2) ans
    where ans = states2 ++ foldl shatters [bigspoon] states2
          bigspoon = map read block
          -- do a lil pruning, this seems to help by about 18% (aka 33% in n^2-ese)
          states2 = filter notcompletelycontained states
          notcompletelycontained littlespoon = any id $ zipWith (>) b0s c0s ++ zipWith (<) b1s c1s
              where [b0s,b1s] = transpose $ chunksOf 2 bigspoon
                    [c0s,c1s] = transpose $ chunksOf 2 littlespoon

volume [x0,x1,y0,y1,z0,z1] = (x1-x0+1) * (y1-y0+1) * (z1-z0+1)

solve = sum . map volume . foldl switch [] . zip [0..]

main = do input <- map words <$> lines <$> readFile "input.txt"
          print $ solve $ take 20 input
          print $ solve input
