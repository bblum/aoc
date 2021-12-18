import Data.List
import Data.Maybe
graph = [([0,1],198), ([0,2],16), ([0,3],178), ([0,4],28), ([0,5],64), ([0,6],246), ([0,7],226), ([1,2],206), ([1,3],56), ([1,4],214), ([1,5],234), ([1,6],56), ([1,7],36), ([2,3],194), ([2,4],36), ([2,5],80), ([2,6],254), ([2,7],234), ([3,4],194), ([3,5],214), ([3,6],72), ([3,7],64), ([4,5],68), ([4,6],262), ([4,7],242), ([5,6],282), ([5,7],262), ([6,7],36)]
main = do print $ minimum $ map (sum . map (fromJust . flip lookup graph) . map (sort . take 2) . drop 2 . reverse . tails .  (0:)) $ permutations [1..7]
          print $ minimum $ map (sum . map (fromJust . flip lookup graph) . map (sort . take 2) . drop 2 . reverse . tails . (++[0]) . (0:)) $ permutations [1..7]
