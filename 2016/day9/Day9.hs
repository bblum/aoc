import Control.Arrow
import Data.List.Extra
import Data.Maybe

decompress pt2 ('(':s) =
    let (x,(y,r)) = read *** first read $ second (fromJust . stripInfix ")") $ fromJust $ stripInfix "x" s
    in (decompress pt2 $ drop x r) + if pt2 then y * (decompress pt2 $ take x r) else x * y
decompress pt2 (_:s) = 1 + decompress pt2 s
decompress pt2 [] = 0

main = interact $ (++"\n") . show . (decompress False &&& decompress True) . trim
