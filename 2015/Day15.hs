import Data.List
import Control.Arrow

ingredience = map ((head &&& drop 1) . reverse) [[2,0,-2,0,3], [0,5,-3,0,3], [0,0,5,-1,8], [0,-1,0,5,8]]

partitions 0       gold = undefined
partitions pirates 0    = [replicate pirates 0]
partitions 1       gold = [[gold]]
partitions pirates gold = concatMap (\g -> map (g:) $ partitions (pirates-1) (gold-g)) [0..gold]

score = product . map (max 0 . sum) . transpose . zipWith (\i n-> map (n*) $ snd $ ingredience !! i) [0..]

calories = sum . zipWith (\i n -> n * fst (ingredience !! i)) [0..]

main = print $ maximum $ filter ((==500) . snd) $ map (score &&& calories) $ partitions (length ingredience) 100
