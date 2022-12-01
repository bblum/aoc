import Data.List

make input = map mkrow [0..maxy+1]
    where [maxx,maxy] = map maximum $ transpose input
          mkrow y = map (flip elem input . (:[y])) [0..maxx]

fold input ["x",x] = transpose $ fold (transpose input) ["y",x]
fold input ["y",y] = zipWith (zipWith (||)) (pad bot top) $ pad top $ reverse bot
    where (top,_:bot) = splitAt (read y) input
          pad other me = replicate (length other - length me) (replicate (length $ head me) False) ++ me

render True = '#'
render False = ' '

main = do input <- map (map read . words) <$> lines <$> readFile "input.txt"
          folds <- map words <$> lines <$> readFile "folds.txt"
          print $ length $ filter id $ concat $ fold (make input) $ head folds
          mapM print $ map (map render) $ foldl fold (make input) folds
