import qualified Data.Map as M
import Data.List
import Data.Maybe
import Control.Applicative
import Control.Monad

parse [a,b,c,')',d,e,f] = ([a,b,c],[d,e,f])

buildMap orbitMap (thing,moon) = M.insertWith (++) thing [moon] orbitMap

data System = S {
    size :: Int,
    orbits :: Int,
    you :: Maybe Int,
    santa :: Maybe Int,
    answer :: Maybe Int
}

buildSystem input name =
    let moons = map (buildSystem input) $ M.findWithDefault [] name input
        totalOrbits = sum $ map orbits moons
        totalSize = sum $ map size moons
        depth who who_fn = if name == who then Just (-1)
                           else fmap (+1) $ join $ find isJust $ map who_fn moons
        youDepth = depth "YOU" you
        sanDepth = depth "SAN" santa
        moonAnswer = join $ find isJust $ map answer moons
        thisAnswer = do you <- youDepth; san <- sanDepth; Just $ you + san
    in S (1 + totalSize) (totalOrbits + totalSize) youDepth sanDepth (moonAnswer <|> thisAnswer)

main = do input <- map parse <$> lines <$> readFile "input.txt"
          let system = buildSystem (foldl buildMap M.empty input) "COM"
          print $ orbits system
          print $ fromJust $ answer system
