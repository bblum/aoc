import Data.List
import Data.Bits
import Data.Char
import Numeric

input1 = [14,58,0,116,179,16,1,104,2,254,167,86,255,55,122,244]
input2 = map fromEnum "14,58,0,116,179,16,1,104,2,254,167,86,255,55,122,244"

rot n list = drop n list ++ take n list
chunk n list = if length list <= n then [list] else take n list : chunk n (drop n list)

move ((pos,skip),list) len = (((pos + len + skip) `mod` length list, skip + 1), newlist)
    where view = (reverse $ take len $ rot pos list) ++ (drop len $ rot pos list)
          newlist = rot (length list - pos) view

hash :: Int -> ((Int, Int), [Int]) -> [Int]
hash 0 state = map (foldl xor 0) $ chunk 16 $ snd state
hash n state = hash (n-1) $ foldl move state $ input2 ++ [17, 31, 73, 47, 23]

hexify x = reverse $ take 2 $ reverse $ '0':showIntAtBase 16 intToDigit x ""

main = do print $ product $ take 2 $ snd $ foldl move ((0,0),[0..255]) input1
          print $ concat $ map hexify $ hash 64 ((0,0),[0..255])
