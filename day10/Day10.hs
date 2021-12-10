import Data.List
import Data.Maybe

match '(' ')' = True
match '[' ']' = True
match '{' '}' = True
match '<' '>' = True
match _ _ = False

parse stack (r:rest) | elem r "([{<" = parse (r:stack) rest
parse (s:stack) (r:rest) = if match s r then parse stack rest else [r]
parse [] (r:rest) = [r]
parse stack [] = stack

score ")" = 3
score "]" = 57
score "}" = 1197
score ">" = 25137
score _ = 0

autocomplete = foldl (\n c -> n * 5 + (fromJust $ findIndex (==c) " ([{<")) 0

middle xs = xs !! div (length xs) 2

main = do input <- map (parse "") <$> lines <$> readFile "input.txt"
          print $ sum $ map score input
          print $ middle $ sort $ map autocomplete $ filter ((== 0) . score) input
