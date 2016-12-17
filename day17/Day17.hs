import Crypto.Hash.MD5
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base16 (encode)
import Data.List
import Data.Maybe
import Control.Monad
import Control.Arrow

nbrs (_,path) = map fst $ filter (flip elem "bcdef" . snd) $ zip "UDLR" $ unpack $ encode $ hash $ pack $ "udskfozm" ++ path

step ((3,3),path) dir = Nothing
step ((x,y),path) 'U' = guard (y /= 0) >> Just ((x,y-1), path ++ "U")
step ((x,y),path) 'D' = guard (y /= 3) >> Just ((x,y+1), path ++ "D")
step ((x,y),path) 'L' = guard (x /= 0) >> Just ((x-1,y), path ++ "L")
step ((x,y),path) 'R' = guard (x /= 3) >> Just ((x+1,y), path ++ "R")

result = snd . fromJust . find ((== (3,3)) . fst) . fromJust . find (any ((== (3,3)) . fst))

main = print $ (result &&& length . result . reverse . takeWhile (not . null))
                $ iterate (concatMap $ liftM2 mapMaybe step nbrs) [((0,0),"")]
