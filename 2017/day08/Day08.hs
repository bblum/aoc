import Data.Map (empty,findWithDefault,alter)
import Data.Maybe
import Control.Arrow

cmp c = case c of "=="->(==); "<="->(<=); ">="->(>=); "!="->(/=); ">"->(>); "<"->(<)
oper o = case o of "inc"->(+); "dec"->(-)

exec (m0,maxval) [r,o,x,"if",r0,c,y] = (m, max maxval $ maximum m)
    where f rv = Just $ oper o (fromMaybe 0 rv) (read x)
          m = if cmp c (findWithDefault 0 r0 m0) (read y) then alter f r m0 else m0

main = interact $ (++"\n") . show . first maximum . foldl exec (empty, 0) . map words . lines
