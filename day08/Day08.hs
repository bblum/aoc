import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Arrow

cmp c = fromJust $ lookup c [("==",(==)),("<=",(<=)),(">=",(>=)),("!=",(/=)),(">",(>)),("<",(<))]
oper o = fromJust $ lookup o [("inc",(+)),("dec",(-))]

exec (m0,maxval) [r,o,x,"if",r0,c,y] = (m, max maxval $ maximum m)
    where f rv = Just $ oper o (fromMaybe 0 rv) (read x)
          m = if cmp c (M.findWithDefault 0 r0 m0) (read y) then M.alter f r m0 else m0

main = interact $ (++"\n") . show . first maximum . foldl exec (M.empty, 0) . map words . lines
