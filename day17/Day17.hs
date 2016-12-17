import Data.List
import Data.Maybe
import Data.Char
import Crypto.Hash.MD5
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base16 (encode)

filterdirs (x,y) =
    let f1 = if y == 0 then filter (/= 'U') else id
        f2 = if x == 0 then filter (/= 'L') else id
        f3 = if x == 3 then filter (/= 'R') else id
        f4 = if y == 3 then filter (/= 'D') else id
    in f1 . f2 . f3 . f4

nbrs (3,3) _ = []
nbrs (x,y) path = filterdirs (x,y) $ map fst $ filter (flip elem "bcdef" . snd) $ zip "UDLR" $ take 4 $ unpack $ encode $ hash $ pack $ "udskfozm" ++ path

move 'U' (x,y) = (x,y-1)
move 'R' (x,y) = (x+1,y)
move 'L' (x,y) = (x-1,y)
move 'D' (x,y) = (x,y+1)

step (pos,path) = map (\dir -> (move dir pos, path ++ [dir])) $ nbrs pos path

-- part 1
-- main = print $ find (any ((== (3,3) . fst))) $ iterate (concatMap step) [((0,0),"")]
-- part 2
main = print $ find (any ((== (3,3)) . fst)) $ reverse $ takeWhile (not . null) $ iterate (concatMap step) [((0,0),"")]
