{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

data Reindeer = Reindeer { name :: String, speed :: Int, stamina :: Int, cooldown :: Int }

noobs = [
    Reindeer "Vixen" 8     8   53  , 
    Reindeer "Blitzen" 13  4   49  , 
    Reindeer "Rudolph" 20  7   132 , 
    Reindeer "Cupid" 12    4   43  , 
    Reindeer "Donner" 9    5   38  , 
    Reindeer "Dasher" 10   4   37  , 
    Reindeer "Comet" 3     37  76  , 
    Reindeer "Prancer" 9   12  97  , 
    Reindeer "Dancer" 37   1   36  
    ]

fly (Reindeer _ spd stam rest) = take 2503 $ cycle $ replicate stam spd ++ replicate rest 0

score distances = map (\x -> if x == maximum distances then 1 else 0) distances

main = do print $ maximum $ map (sum . fly) noobs
          print $ maximum $ map sum $ transpose $ map score $ transpose $ map (drop 1 . scanl (+) 0 . fly) noobs
