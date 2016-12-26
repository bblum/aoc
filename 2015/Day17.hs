{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

input = [43,3,4,10,21,44,4,6,47,41,34,17,17,44,36,31,46,9,27,38]
powerset = filterM $ const [True, False]

main = print $ length $ filter ((== 150) . sum) $ filter ((==4) . length) $ powerset input
