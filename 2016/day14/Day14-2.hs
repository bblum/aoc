import Crypto.Hash.MD5
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base16 (encode)
import Data.List
import Data.Maybe

gethash stretch salt = (salt, unpack $ iterate (encode . hash) (pack $ "ngcjuoqr" ++ show salt) !! stretch)

solve ((salt, hash):rest) =
    do triple <- find ((>2) . length) $ group hash
       find (isInfixOf $ replicate 5 $ head triple) $ map snd $ take 1000 rest
       return salt

main = print $ map ((!! 63) . catMaybes . map solve . tails . flip map [0..] . gethash) [1,2017]
