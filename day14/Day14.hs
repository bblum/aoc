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

type Ingredient = (Int, String)

parse_ingredient :: String -> Ingredient
parse_ingredient i = (read quantity, name)
    where [quantity, name] = words i

parse :: String -> (String, (Int, [Ingredient]))
parse line = (output, (quantity, ingredience))
    where [left,right] = splitOn " => " line
          ingredience = map parse_ingredient $ splitOn ", " left
          (quantity, output) = parse_ingredient right

findremove :: String -> [Ingredient] -> Maybe (Ingredient, [Ingredient])
findremove name l = fmap (\x -> (x, l \\ [x])) $ find ((== name) . snd) l

add_goal :: [Ingredient] -> Ingredient -> [Ingredient]
add_goal goals (quantity,name) =
    case findremove name goals of
        Just ((existing_quantity, _), rest) -> (existing_quantity + quantity, name) : rest
        Nothing -> (quantity, name) : goals

try_use_excess :: [Ingredient] -> Ingredient -> (Int, [Ingredient])
try_use_excess excess (quantity, name) =
    case findremove name excess of
        Just ((excess_quantity, _), rest) | excess_quantity > quantity -> traceShow ("use excess",name,excess_quantity,quantity) $ (0, (excess_quantity - quantity, name) : rest)
        Just ((excess_quantity, _), rest) | excess_quantity <= quantity -> traceShow ("use excess",name,excess_quantity,quantity) $ (quantity - excess_quantity, rest)
        Nothing -> (quantity, excess)

solve result excess [] recipes = result
solve result excess ((quantity,"ORE"):goals) recipes = traceShow ("solve ore",quantity) $ solve (result + quantity) excess goals recipes
solve result excess ((quantity,goal):goals) recipes =
    case try_use_excess excess (quantity, goal) of
        (0, newexcess) -> traceShow ("solve with excess", quantity, goal, newexcess) $ solve result newexcess goals recipes
        (newquantity, newexcess) -> traceShow ("solve normally", quantity, goal, newquantity, newexcess,newgoals) $ solve result newexcess2 newgoals recipes
            where (r_quantity, recipe) = recipes M.! goal
                  num_recipes = ceiling $ fromIntegral newquantity / fromIntegral r_quantity
                  num_excess_produced = (r_quantity * num_recipes) - newquantity
                  newexcess2 = if num_excess_produced == 0 then newexcess else (num_excess_produced, goal):newexcess
                  newgoals = foldl add_goal goals $ map (first (* num_recipes)) recipe

main = do recipes <- map parse <$> lines <$> readFile "input.txt"
          print $ solve 0 [] [(1,"FUEL")] $ M.fromList recipes
