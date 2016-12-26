import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

range [x1,y1,"through",x2,y2] = (,) <$> [read x1..read x2] <*> [read y1..read y2]

toggle x s = if S.member x s then S.delete x s else S.insert x s

cmd :: S.Set (Int, Int) -> [String] -> S.Set (Int, Int)
cmd lights ("toggle":rest) = foldr toggle lights $ range rest
cmd lights ("turn":"on":rest) = foldr S.insert lights $ range rest
cmd lights ("turn":"off":rest) = foldr S.delete lights $ range rest

main = interact $ (++"\n") . show . S.size . foldl cmd S.empty . map words . lines
