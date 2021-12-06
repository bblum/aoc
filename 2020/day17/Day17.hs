import qualified Data.Map as M
import Data.List

count = length . filter (== '#')

expand [los,his] = sequence [[lo-1..hi+1] | (lo,hi) <- zip los his]

coordify pfx = M.fromList . concat . zipWith (\y -> zipWith (\x c -> (pfx ++ [y,x],c)) [0..]) [0..]

step state = M.fromList [(p, rule (get p) (count $ map get $ expand [p,p])) | p <- expand bounds]
    where bounds = map ($ transpose $ M.keys state) [map minimum, map maximum]
          get p = maybe '.' id $ M.lookup p state
          rule '#' 4 = '#'
          rule _ 3 = '#'
          rule _ _ = '.'

solve state = count $ M.elems $ iterate step state !! 6

main = do input <- lines <$> readFile "input.txt"
          print $ solve $ coordify [0] input
          print $ solve $ coordify [0,0] input
