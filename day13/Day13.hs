import Data.List
import Data.List.Split

data Packet = I Int | L [Packet] deriving Eq

parseint 'A' = 10
parseint c = read [c]

parselist (',':rest) = parselist rest
parselist (']':rest) = (rest, [])
parselist rest = (x:) <$> parselist rest2
    where (rest2, x) = parse rest

parse (c:rest) | elem c "0123456789A" = (rest, I $ parseint c)
parse ('[':rest) = L <$> parselist rest

instance Ord Packet where
    I i1 <= I i2 = i1 <= i2
    I i1 <= L l2 = L [I i1] <= L l2
    L l1 <= I i2 = L l1 <= L [I i2]
    L [] <= L _ = True
    L _ <= L [] = False
    L (x1:l1) <= L (x2:l2) | x1 <= x2 && x2 <= x1 = L l1 <= L l2
    L (x1:l1) <= L (x2:l2) = x1 <= x2

part1 (_,[p1,p2]) = p1 <= p2

dividers = [L [L [I 2]], L [L [I 6]]]

main = do pairs <- map (map $ snd . parse) <$> splitOn [[]] <$> lines <$> readFile "input.txt"
          print $ sum $ map fst $ filter part1 $ zip [1..] pairs
          print $ product $ map fst $ filter (flip elem dividers . snd) $ zip [1..] $ sort $ dividers ++ concat pairs
