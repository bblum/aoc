{-# LANGUAGE FlexibleContexts, TupleSections, MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Char
import Data.Ord
import Control.Monad.State
import Control.Arrow
import Debug.Trace

count f = length . filter f

parse :: [String] -> [[(String, String)]]
parse input = map passportify $ splitOn [""] input
    where passportify l = map parseline l
          parseline line = let [a,b] = splitOn ":" line in (a,b)

contains :: String -> [(String, a)] -> Bool
contains k l = case find ((== k) . fst) l of Just _ -> True; Nothing -> False

validkey k l = case find ((== k) . fst) l of Just (_,v) -> vk v k; Nothing -> False
    where between v0 lower upper = let v = read v0 in v >= lower && v <= upper
          vk :: String -> String -> Bool
          vk v0 "byr" = between v0 1920 2002
          vk v0 "iyr" = between v0 2010 2020
          vk v0 "eyr" = between v0 2020 2030
          vk [x,y,z,'c','m'] "hgt" = between [x,y,z] 150 193
          vk [x,y,'i','n'] "hgt" = between [x,y] 59 76
          vk _ "hgt" = False
          vk ['#',a,b,c,d,e,f] "hcl" = all (flip elem "0123456789abcdef") [a,b,c,d,e,f]
          vk _ "hcl" = False
          vk v0 "ecl" = elem v0 ["amb","blu","brn","gry","grn","hzl","oth"]
          vk v0 "pid" = length v0 == 9 && all (flip elem "0123456789") v0

valid :: [(String, String)] -> Bool
valid l = contains "byr" l && contains "iyr" l && contains "eyr" l && contains "hgt" l && contains "hcl" l && contains "ecl" l && contains "pid" l

valid2 l = validkey "byr" l && validkey "iyr" l && validkey "eyr" l && validkey "hgt" l && validkey "hcl" l && validkey "ecl" l && validkey "pid" l

main = do input <- parse <$> lines <$> readFile "input.txt"
          print $ count valid2 input
