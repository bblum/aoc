import qualified Data.Map as M
import Data.List
import Data.List.Split
import Control.Arrow

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
        Just ((excess_quantity, _), rest) | excess_quantity > quantity -> (0, (excess_quantity - quantity, name) : rest)
        Just ((excess_quantity, _), rest) | excess_quantity <= quantity -> (quantity - excess_quantity, rest)
        Nothing -> (quantity, excess)

solve result excess [] recipes = result
solve result excess ((quantity,"ORE"):goals) recipes = solve (result + quantity) excess goals recipes
solve result excess ((quantity,goal):goals) recipes =
    case try_use_excess excess (quantity, goal) of
        (0, newexcess) -> solve result newexcess goals recipes
        (newquantity, newexcess) -> solve result newexcess2 newgoals recipes
            where (r_quantity, recipe) = recipes M.! goal
                  num_recipes = ceiling $ fromIntegral newquantity / fromIntegral r_quantity
                  num_excess_produced = (r_quantity * num_recipes) - newquantity
                  newexcess2 = if num_excess_produced == 0 then newexcess else (num_excess_produced, goal):newexcess
                  newgoals = foldl add_goal goals $ map (first (* num_recipes)) recipe

part1 n recipes = solve 0 [] [(n,"FUEL")] recipes

-- found answer
part2 goal lower_bound (Just upper_bound) recipes | lower_bound + 1 == upper_bound = lower_bound
part2 goal lower_bound (Just upper_bound) recipes =
    -- search the interval with normal binary search
    let midpoint = div (lower_bound + upper_bound) 2
    in if part1 midpoint recipes > goal then
           part2 goal lower_bound (Just midpoint) recipes
       else
           part2 goal midpoint (Just upper_bound) recipes
part2 goal lower_bound Nothing recipes =
    -- try to find an upper bound with exponential binary search
    if part1 (lower_bound * 2) recipes > goal then
        part2 goal lower_bound (Just $ lower_bound * 2) recipes
    else
        part2 goal (lower_bound * 2) Nothing recipes

main = do recipes <- map parse <$> lines <$> readFile "input.txt"
          print $ part1 1 $ M.fromList recipes
          print $ part2 1000000000000 1 Nothing $ M.fromList recipes
