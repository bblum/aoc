import qualified Data.Set as S
import Data.List
import Data.List.Split
import Data.Tuple
import Data.Maybe
import Control.Monad

overlaprequired = 12
pairwiseoverlaprequired = length $ pairs [1..overlaprequired] -- 66

pairs l = [ (l !! i, l !! j) | i <- [0..length l-1], j <- [0..i-1] ]

pairwisedistances :: [Coord] -> [((Coord,Coord), Coord)]
pairwisedistances = map distance . pairs
    where distance (b1,b2) = ((b1,b2), sort $ map abs $ zipWith subtract b1 b2)

overlaps = map fst . filter ((>=pairwiseoverlaprequired) . snd) . map overlap . pairs . zip [0..]
    where overlap ((i,s1),(j,s2)) = ((i,j), S.size $ foldl1 S.intersection $ map (S.fromList . map snd . pairwisedistances) [s1,s2])

-- generates the cube symmetry group on n-dimensional coordinates (n! * 2^n, so n=3 -> 48 fns)
-- this returns functions so you can apply the same one consistently to a group of coordinates
-- half of these will be mirror images, hopefully that won't be a problem for the puzzle input
--
-- e.g. map ($ [1..]) (symmetries 2) -> [[-1,-2],[-1,2],[1,-2],[1,2],[-2,-1],[-2,1],[2,-1],[2,1]]
symmetries :: Int -> [[Int] -> [Int]]
symmetries n = [\l -> map (\i -> (if elem i mirrors then negate else id) (l !! (order !! i))) [0..n-1]
                | order <- permutations [0..n-1],
                  mirrors <- filterM (const [True,False]) [0..n-1]]

type Coord = [Int]
type Input = [[Coord]]
type Output = [(Int,(Coord,[Coord]))] -- id, normalized coordinate of origin, normalized data

stitch :: [(Int,Int)] -> Input -> [Int] -> Output -> Output
stitch os input [] output = output
stitch os input (i:q) output = stitch os input (nub $ q ++ unexplored) ((i,ans):output)
    where nbrs = map snd $ filter ((==i) . fst) $ os ++ map swap os
          unexplored = filter (not . flip elem (map fst output)) nbrs
          -- decide arbitrarily
          (nbrid,(_,nbrbeacons)) = head $ filter (flip elem nbrs . fst) output
          mybeacons = input !! i
          -- compute this again because whatever
          mypairs = pairwisedistances mybeacons
          nbrpairs = pairwisedistances nbrbeacons
          -- we should be using `nub . concat` here to get ALL common pairs, but we dont need to;
          -- the first matching pair suffices to determine which symmetry fn to use
          [(m1,n1),(m2,n2)] = head [ if match m1 n1 then [(m1,n1),(m2,n2)] else [(m1,n2),(m2,n1)]
                                     | ((m1,m2),md) <- mypairs, ((n1,n2),nd) <- nbrpairs, md == nd ]
          match mp np = S.size (S.intersection (edgeset mp mypairs) (edgeset np nbrpairs)) >= overlaprequired - 1
          edgeset p = S.fromList . map snd . filter (\((p1,p2),_) -> p == p1 || p == p2)
          -- make sure to apply the symmetry on MYSELF -- nbr is already fixed
          fits f = if zipWith (+) myorigin (f m2) == n2 then Just (f,myorigin) else Nothing
              where myorigin = zipWith (-) n1 $ f m1
          (symmetry,myorigin) = head $ mapMaybe fits $ symmetries 3
          ans = (myorigin, map (zipWith (+) myorigin . symmetry) mybeacons)

solve g = maximum [ sum $ zipWith ((abs .) . (-)) s1 s2 | (_,(s1,_)) <- g, (_,(s2,_)) <- g ]

main = do input <- splitOn [[]] <$> map (map read . words) <$> lines <$> readFile "input.txt"
          let os = overlaps input
          print $ (length $ concat input) - 12 * (length os) -- part 1, wow, it worked
          -- (6 is 0's only neighbor; determined by inspection; sue me)
          print $ solve $ stitch os input [6] [(0,([0,0,0], head input))]
