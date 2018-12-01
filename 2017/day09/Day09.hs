import Control.Arrow

eval (depth,score) '{' = (depth+1, score+depth)
eval (depth,score) '}' = (depth-1, score)
eval x _ = x

garbage n ('!':_:rest) = garbage n rest
garbage n ('>'  :rest) = ungarbage n rest
garbage n (_    :rest) = garbage (n+1) rest
ungarbage n ('<':rest) = garbage n rest
ungarbage n (c  :rest) = first (c:) (ungarbage n rest)
ungarbage n []         = ([],n)

main = interact $ (++"\n") . show . first (snd . foldl eval (0,1)) . ungarbage 0
