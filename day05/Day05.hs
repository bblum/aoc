{-# LANGUAGE FlexibleContexts, TupleSections, MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

solve = undefined

react [] = []
react [y] = [y]
react (x:y:rest) =
    if x /= y && toUpper x == toUpper y then react rest else x:(react (y:rest))

reactlots foo =
    let bar = react foo in if length bar == length foo then bar else reactlots bar

remove input noob = reactlots $ filter (\x -> x /= noob && x /= toUpper noob) input

main = do input <- filter (/= '\n') <$> readFile "input.txt"
          -- print $ reactlots input
          print $ length $ reactlots input
          let result = map (remove input) "abcdefghijklmonpqrstuvwxyz"
          print $ map length result
          print $ minimum $ map length result

