import Crypto.Hash.MD5
import Data.ByteString.Char8 (pack, unpack)
import Data.Hex
import Data.Maybe
import Data.List

digit hash = if isPrefixOf "00000" hash then Just (hash !! 5, hash !! 6) else Nothing

crack result position = fromJust $ lookup position result

main = putStrLn $ flip map "01234567" . crack $ mapMaybe digit $ map (unpack . hex . hash . pack . ("cxdnnyjw" ++) . show) [0..]
