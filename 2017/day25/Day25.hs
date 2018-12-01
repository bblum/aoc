import qualified Data.Map as M
import Data.Maybe

statemachine 0 0 = (1,True,1)
statemachine 0 1 = (0,False,2)
statemachine 1 0 = (1,False,0)
statemachine 1 1 = (1,False,3)
statemachine 2 0 = (1,True,3)
statemachine 2 1 = (0,True,2)
statemachine 3 0 = (0,False,1)
statemachine 3 1 = (0,True,4)
statemachine 4 0 = (1,True,2)
statemachine 4 1 = (1,False,5)
statemachine 5 0 = (1,False,4)
statemachine 5 1 = (1,True,0)

ture ((i,s),tape) = ((i + if dir then 1 else -1, s'), M.insert i v tape)
    where (v,dir,s') = statemachine s $ fromMaybe 0 $ M.lookup i tape

main = print $ M.size $ M.filter (==1) $ snd $ iterate ture ((0,0), M.empty) !! 12656374
