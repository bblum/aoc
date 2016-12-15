import Data.List
import Data.Maybe

-- input = [(17,15),(3,2),(19,4),(13,2),(7,2),(5,0),(11,0)]
input = [(101,2),(163,7),(263,10),(293,2),(373,9),(499,0),(577,0)]

crt (base,incr) (prime, remainder) =
    (fromJust $ find ((== remainder) . (`mod` prime)) [base, base+incr..], incr * prime)

main = print $ fst $ foldl crt (0,1) $ zipWith (\d (w,p) -> (w, (-p-d) `mod` w)) [1..] input
