{-# LANGUAGE TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

range [x1,y1,"through",x2,y2] = (,) <$> [read x1..read x2] <*> [read y1..read y2]

cmd :: M.Map (Int, Int) Int -> [String] -> M.Map (Int, Int) Int
cmd lights ("toggle":rest) = foldr (M.adjust (+2)) lights $ range rest
cmd lights ("turn":"on":rest) = foldr (M.adjust (+1)) lights $ range rest
cmd lights ("turn":"off":rest) = foldr (M.adjust (max 0 . subtract 1)) lights $ range rest

alloff = M.fromList $ map (,0) $ range ["0","0","through","999","999"]

main = interact $ (++"\n") . show . sum . M.elems . foldl cmd alloff . map words . lines
