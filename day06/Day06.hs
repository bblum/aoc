import qualified Data.Map as M
import Data.List
import Data.Maybe

redistribute mem =
    let l = length mem
        m = maximum mem
        i = fromJust $ findIndex (== m) mem
        stuff = zipWith (+) (replicate l $ div m l) (replicate (mod m l) 1 ++ repeat 0)
        (mem1,mem2) = splitAt i mem
    in zipWith (+) (mem1 ++ [0] ++ drop 1 mem2) (drop (l-1-i) stuff ++ take (l-1-i) stuff)

step time m list = if M.member list m then (time, m M.! list)
                   else step (time+1) (M.insert list time m) (redistribute list)

main = print $ let (x,y) = step 0 M.empty [5,1,10,0,1,7,13,14,3,12,8,10,7,12,0,6] in (x,x-y)
