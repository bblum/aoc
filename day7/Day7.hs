import Data.List.Utils (hasAny)
import Data.Tuple (swap)
import Control.Arrow ((***))
import Control.Monad (join)
import Control.Applicative (liftA)

abba (a:rest@(b:c:d:_)) = (a == d && b == c && a /= b) || abba rest
abba _ = False

aba r (a:rest@(b:c:_)) = if (a == c && a /= b) then (a,b):(aba r rest) else aba r rest
aba r _ = r

supportsTLS = uncurry ((. not) . (&&)) . join (***) (any abba)
supportsSSL = uncurry hasAny . liftA (map swap) . join (***) (concat . map (aba []))

parse c (x:xs,ys) = if c == '[' || c == ']' then ("":ys, x:xs) else ((c:x):xs, ys)

main = interact $ (++"\n") . show . (length . filter supportsTLS *** length . filter supportsSSL) . join (,) . map (foldr parse ([""],[])) . lines
