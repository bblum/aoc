import Data.Array.ST
import Control.Monad.ST

exec :: Int -> Int -> (STArray s Int Int) -> ST s Int
exec steps pos program =
    do size <- snd <$> getBounds program
       if pos > size then return steps
       else do val <- readArray program pos
               writeArray program pos $ if val >= 3 then val-1 else val+1
               exec (steps+1) (pos+val) program

main = do input <- map read <$> lines <$> readFile "input.txt"
          print $ runST $ do program <- newListArray (0, length input-1) input
                             exec 0 0 program
