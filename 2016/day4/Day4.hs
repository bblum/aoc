import Data.List
import Data.Char
import GHC.Exts
import Control.Arrow

check [text, _, hash] = isPrefixOf hash $ map head $ sortWith ((0-) . length) $ group $ sort text

decode' key char = chr $ ord 'a' + ((ord char - ord 'a' + key) `mod` 26)
decode [text, key, _] = (map (decode' $ read key) text, key)

main = interact $ (++"\n") . show . (sum . map (read . (!! 1)) &&& find (isPrefixOf "north" . fst) . map decode) . filter check . map words . lines
