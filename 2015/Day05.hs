{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

nice1 str = (length $ filter (flip elem "aeiou") str) >= 3 &&
            (any (uncurry (==)) $ zip str $ tail str) &&
            (not $ any id $ map (flip isInfixOf str) ["ab","cd","pq","xy"])

pair (x:y:rest) = isInfixOf [x,y] rest || pair (y:rest)
pair _ = False

bs (x:rest@(y:z:_)) = x == z || bs rest
bs _ = False

nice2 str = pair str && bs str

main = interact $ (++"\n") . show . (length . filter nice1 &&& length . filter nice2) . lines
