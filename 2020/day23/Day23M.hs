import Control.Monad.ST
import Data.Array.ST
import Data.List

input = [5,8,3,9,7,6,2,4,1]

-- i guess now i know why control monad doesn't include this mf
iterateM 0 f s = return [s]
iterateM n f s = (s:) <$> (iterateM (n-1) f =<< f s)

-- STArray: 18-20 sec; STUArray: 13-14 sec
crabM :: Int -> (Int, STUArray s Int Int) -> ST s (Int, STUArray s Int Int)
crabM n (c0, cups) =
    do [_,c1,c2,c3,c4] <- iterateM 4 (readArray cups) c0
       let dest c0 = if elem d [c1,c2,c3] then dest d else d where d = mod (c0-2) n + 1
       next <- readArray cups $ dest c0
       mapM_ (uncurry $ writeArray cups) [(c0,c4),(dest c0,c1),(c3,next)]
       newc0 <- readArray cups c0
       return (newc0, cups)

solveM :: Int -> Int -> Int -> ST s [Int]
solveM p n k =
    do let l = input ++ [10..n]
       initial <- newListArray (1,n) $ map snd $ sortOn fst $ zip (last l:l) l
       (_,final) <- last <$> iterateM k (crabM n) (head l, initial)
       tail <$> iterateM p (readArray final) 1

main = do putStrLn $ concatMap show $ runST $ solveM 8 9 100
          print $ product $ runST $ solveM 2 1000000 10000000
