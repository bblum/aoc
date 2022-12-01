import Data.List
import Data.Maybe

match '(' = ')'
match '[' = ']'
match '{' = '}'
match '<' = '>'

parse stack (r:rest) | elem r "([{<" = parse (r:stack) rest
parse (s:stack) (r:rest) | match s == r = parse stack rest
parse stack [] = stack
parse _ (r:rest) = [r]

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
