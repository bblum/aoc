import Crypto.Hash.MD5
import Data.ByteString.Char8 (pack, unpack)
import Data.Hex
import Data.Maybe
import Data.List

digit hash = if isPrefixOf "00000" hash then Just $ hash !! 5 else Nothing

main = putStrLn $ take 8 $ mapMaybe digit $ map (unpack . hex . hash . pack . ("cxdnnyjw" ++) . show) [0..]
