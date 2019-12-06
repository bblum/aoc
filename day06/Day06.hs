{-# LANGUAGE FlexibleContexts, TupleSections, MonadComprehensions #-}
import qualified Data.Map as M
import qualified Data.Set as S
import Data.List
import Data.Maybe
import Data.Char
import Data.Ord
import Control.Applicative
import Control.Monad.State
import Control.Arrow
import Debug.Trace

parse [a,b,c,')',d,e,f] = ([a,b,c],[d,e,f])

build orbitMap (thing,moon) = M.insertWith (++) thing [moon] orbitMap

data System = S {
    size :: Int,
    orbits :: Int,
    moons :: [System],
    you :: Maybe Int,
    santa :: Maybe Int,
    answer :: Maybe Int
}

buildsystem directorbits name =
    let moonList = if M.member name directorbits then directorbits M.! name else []
        moonSystems = map (buildsystem directorbits) moonList
        totalOrbits = sum $ map orbits moonSystems
        totalSize   = sum $ map size moonSystems
        depth :: String -> (System -> Maybe Int) -> Maybe Int
        depth who who_fn = if name == who then Just (-1)
                           else fmap (+1) $ join $ find isJust $ map who_fn moonSystems
        youDepth :: Maybe Int
        youDepth = depth "YOU" you
        sanDepth :: Maybe Int
        sanDepth = depth "SAN" santa
        moonAnswer = join $ find isJust $ map answer moonSystems
        thisAnswer = do you <- youDepth; san <- sanDepth; Just $ you + san
        bestAnswer = moonAnswer <|> thisAnswer
    in S (1 + totalSize) (totalOrbits + totalSize) moonSystems youDepth sanDepth bestAnswer

solve input =
    let directorbits = foldl build M.empty input
        system = buildsystem directorbits "COM"
    in (orbits system, answer system)

main = do input <- map parse <$> lines <$> readFile "input.txt"
          print $ solve input
