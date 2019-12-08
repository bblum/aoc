{-# LANGUAGE FlexibleContexts, TupleSections, MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord
import Control.Monad.State
import Control.Arrow
import Debug.Trace
import Data.List.Split

width = 25
height = 6

-- solve :: [Int] -> Int
-- solve nums =
--     let numlayers = length nums `div` (width * height)
--         layers = chunksOf (width * height) nums
--         layerswithzeroes = map (\layer -> ((length $ filter (==0) layer), layer)) layers
--         minzeroes = snd $ minimumBy (comparing fst) layerswithzeroes
--     in (length (filter (==1) minzeroes) * length (filter (==2) minzeroes))

asdf 2 x = x
asdf z x = z
merge image layer = zipWith asdf image layer

solve :: [Int] -> [[Int]]
solve nums =
    let layers = chunksOf (width * height) nums
        alltransp = replicate (width * height) 2
    in chunksOf width $ foldl merge alltransp layers

        

main = do input <- head <$> lines <$> readFile "input.txt"
          let nums = map read $ map (\c -> [c]) input
          -- print $ solve nums
          mapM_ print $ solve nums
