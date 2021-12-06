import qualified Data.Map as M
import Data.List
import Control.Monad.State

diffs [ones,threes] (x,y) = case y-x of
    1 -> [ones+1,threes]
    2 -> [ones,threes]
    3 -> [ones,threes+1]

arrange :: [Int] -> Int -> State (M.Map Int Int) Int
arrange adapters 0 = return 1
arrange adapters current =
    do seen <- get
       case M.lookup current seen of
           Just ans -> return ans
           Nothing ->
               do let targets = filter (flip elem adapters) [current-3..current-1]
                  ans <- sum <$> mapM (arrange adapters) targets
                  modify $ M.insert current ans
                  return ans

main = do input <- sort <$> map read <$> lines <$> readFile "input.txt"
          let noobs = 0:input ++ [last input + 3]
          print $ product $ foldl diffs [0,0] $ zip noobs $ tail noobs
          print $ evalState (arrange noobs $ last noobs) M.empty
