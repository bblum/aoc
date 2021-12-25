{-# LANGUAGE TupleSections #-}
import Data.Char

fnbrs8 (y,x) = [(y2,x2) | y2 <- [y-1..y+1], x2 <- [x-1..x+1], (y2,x2) /= (y,x)]

coordify = zipWith (\y -> zipWith ((,) . (y,)) [0..]) [0..]

step f (octos,flashes) = (map unflash octos2, f newflashes flashes)
    where unflash (c,o) = (c, if o >= 10 then 0 else o)
          (octos2,newflashes) = foldl stepone (octos,0) $ map fst octos
stepone (octos,flashes) (y,x) =
    case lookup (y,x) octos of
        Nothing -> (octos,flashes) -- out of bounds
        Just o ->
            if o == 9 then (+1) <$> foldl stepone (octos2,flashes) (fnbrs8 (y,x))
            else (octos2,flashes)
                where octos2 = map (\(yx,o) -> (yx, if yx == (y,x) then o + 1 else o)) octos

part1 input = snd $ iterate (step (+)) (input,0) !! 100
part2 input = fst $ head $ filter ((== length input * length (head input)) . snd . snd) $ zip [0..] $ iterate (step const) (input,1337)

main = do input <- concat <$> coordify <$> map (map digitToInt) <$> lines <$> readFile "input.txt"
          print $ part1 input
          print $ part2 input
