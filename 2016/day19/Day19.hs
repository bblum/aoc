import qualified Data.Sequence as S

steal l = let index = div (S.length l) 2
              newl = S.take index l S.>< S.drop (index+1) l
          in S.drop 1 newl S.>< S.take 1 newl

solve n = flip S.index 0 $ until ((==1) . length) steal $ S.fromList [1..n]

main = print $ solve 3012210
