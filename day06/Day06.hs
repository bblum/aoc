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

parse [a,b,c,')',d,e,f] = ([a,b,c],[d,e,f])

addorbit orbitmap (thing,moon) = M.insert thing newval orbitmap
    where newval = if M.member thing orbitmap then (orbitmap M.! thing) ++ [moon] else [moon]

-- name, numNodes, numOrbits, moons, youDepth, santaDepth
data System = S String Int Int [System] (Maybe Int) (Maybe Int) deriving Show

buildsystem directorbits name =
    let moons = if M.member name directorbits then directorbits M.! name else []
        moonSystems = map (buildsystem directorbits) moons
        totalMoonOrbits = sum $ map (\(S _moonname _nummoons orbits _moonmoons _ _) -> orbits) moonSystems
        totalNoobs = sum $ map (\(S _moonname nummoons _orbits _moonmoons _ _) -> nummoons) moonSystems
        youDepth = if name == "YOU" then Just 0
                   else fmap (+1) $ join $ find isJust $ map (\(S _ _ _ _ youDepth _) -> youDepth) moonSystems
        santaDepth = if name == "SAN" then Just 0
                     else fmap (+1) $ join $ find isJust $ map (\(S _ _ _ _ _ santaDepth) -> santaDepth) moonSystems
    in if isJust youDepth && isJust santaDepth then
           error $ "found: " ++ show youDepth ++ show santaDepth
       else S name (1 + totalNoobs) (totalMoonOrbits + totalNoobs) moonSystems youDepth santaDepth

solve orbits =
    let directorbits = foldl addorbit M.empty orbits
        system = buildsystem directorbits "COM"
        S "COM" numNoobs answer _moons _ _ = system
    in answer

main = do input <- map parse <$> lines <$> readFile "input.txt"
          print $ solve input
