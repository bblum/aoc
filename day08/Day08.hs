{-# LANGUAGE TupleSections #-}
import qualified Data.Set as S
import Data.List
import Data.Char
import Data.Maybe

coordify = zipWith (\y -> zipWith ((,) . (y,)) [0..]) [0..]

vis tallest s ((xy,h):rest) | h > tallest = vis h (S.insert xy s) rest
vis tallest s ((xy,h):rest) = vis tallest s rest
vis tallest s [] = s

scorerow xy ((xy2,h):rest) | xy == xy2 = length visibles + if visibles == rest then 0 else 1
    where visibles = takeWhile ((<h) . snd) rest
scorerow xy (_:rest) = scorerow xy rest

part2 allrows = maximum . map score
    where score xy = product $ map (scorerow xy . fromJust . find (any ((==xy) . fst))) allrows

main = do input <- coordify <$> map (map digitToInt) <$> lines <$> readFile "input.txt"
          let allrows = [input, map reverse input, transpose input, transpose (reverse input)]
          print $ S.size $ foldl (vis (-1)) S.empty $ concat allrows
          print $ part2 allrows $ map fst $ concat input
