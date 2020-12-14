{-# LANGUAGE FlexibleContexts, TupleSections, MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.List.Split
import Data.Bits
import Data.Either
import Data.Maybe
import Data.Char
import Data.Ord
import Control.Monad.State
import Control.Arrow
import Debug.Trace

parse :: [String] -> Either String (Int, Int)
parse ["mask","=",mask] = Left mask
parse ["mem",i,"=",val] = Right (read i :: Int, read val :: Int)

exec :: (String, M.Map Int Int) -> Either String (Int, Int) -> (String, M.Map Int Int)
exec (mask,memory) (Left mask2) = (mask2,memory)
exec (mask,memory) (Right (i,val)) = (mask, M.insert i masked_val memory)
    where masked_val :: Int
          masked_val = foldl maskify val $ zip [0..] $ reverse mask
          maskify val (power,'1') = val .|. (2^power)
          maskify val (power,'0') = val .&. complement (2^power)
          maskify val (power,'X') = val

exec2 :: (String, M.Map Int Int) -> Either String (Int, Int) -> (String, M.Map Int Int)
exec2 (mask,memory) (Left mask2) = (mask2,memory)
exec2 (mask,memory) (Right (i,val)) = (mask, foldr (flip M.insert val) memory i2s)
    where i2s = foldl maskify2 [i] $ zip [0..] $ reverse $ dropWhile (=='0') mask
          -- i2s = traceShow ("mask",i,"&&&",mask,"->",sort i2s0) i2s0
          noob i2 mem = traceShow (i2,"<-",val,mem) $ M.insert i2 val mem

maskify2 :: [Int] -> (Int, Char) -> [Int]
maskify2 is (power,'1') = map (\val -> val .|. (2^power)) is
maskify2 is (power,'0') = is
maskify2 is (power,'Q') = map (\val -> val .&. complement (2^power)) is
maskify2 is (power,'X') = concatMap (\val -> maskify2 [val] (power,'1') ++ maskify2 [val] (power,'Q')) is

main = do input <- map (parse . words) <$> lines <$> readFile "input.txt"
          print $ sum $ M.elems $ snd $ foldl exec (undefined, M.empty) input
          print $ sum $ M.elems $ snd $ foldl exec2 (undefined, M.empty) input
