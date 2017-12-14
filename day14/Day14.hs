{-# LANGUAGE TupleSections #-}
import Data.List
import Data.Char
import Data.Bits
import Control.Monad
import Numeric

rot n list = drop n list ++ take n list
chunk n list = if length list <= n then [list] else take n list : chunk n (drop n list)

move ((pos,skip),list) len = (((pos + len + skip) `mod` length list, skip + 1), newlist)
    where view = (reverse $ take len $ rot pos list) ++ (drop len $ rot pos list)
          newlist = rot (length list - pos) view

hash inp 0 state = map (foldl xor 0) $ chunk 16 $ snd state
hash inp n state = hash inp (n-1) $ foldl move state $ inp ++ [17, 31, 73, 47, 23]

hexify x = reverse $ take 8 $ (reverse $ showIntAtBase 2 intToDigit x "") ++ repeat '0'

knothash inp = concat $ map hexify $ hash inp 64 ((0,0), [0..255] :: [Int])

grid = map (map (=='1') . knothash . map fromEnum . ("vbqugkhl-" ++) . show) [0..127]

coords = concatMap row $ zip grid [0..]
    where row (r,i) = map snd $ filter fst $ zip r $ map (i,) [0..]

bfs coords (i,j) = if elem (i,j) coords then foldl bfs (delete (i,j) coords) [(i+1,j),(i-1,j),(i,j+1),(i,j-1)] else coords

main = do print $ sum $ map (length . filter id) $ grid
          print $ length $ takeWhile (not . null) $ iterate (ap bfs head) coords
