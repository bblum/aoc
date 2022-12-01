{-# LANGUAGE TupleSections #-}
import Data.Maybe
import Control.Applicative

data Snailfish = L (Snailfish, Snailfish) | N Int deriving Eq

explode d (N n) = Nothing
explode 4 (L (N p, N q)) = Just (N 0, (p,q))
explode d (L (p, q)) = try 'l' p <|> try 'r' q
    where try c s = settle c <$> explode (d+1) s
          settle 'l' (p2, (l,r)) = (L (p2, add 'l' r q), (l,0))
          settle 'r' (q2, (l,r)) = (L (add 'r' l p, q2), (0,r))
          add _ i (N n) = N $ n+i
          add 'l' i (L (p,q)) = L (add 'l' i p, q)
          add 'r' i (L (p,q)) = L (p, add 'r' i q)

splitify (N n) = if n > 9 then Just $ L (N $ div n 2, N $ n - div n 2) else Nothing
splitify (L (p,q)) = L <$> ((,q) <$> splitify p <|> (p,) <$> splitify q)

reduce s = fromMaybe s $ reduce <$> (fst <$> explode 0 s <|> splitify s)

add = curry $ reduce . L

magnitude (N n) = n
magnitude (L (p,q)) = 3 * magnitude p + 2 * magnitude q

main = do print $ magnitude $ foldl1 add input
          print $ maximum [ magnitude $ add s1 s2 | s1 <- input, s2 <- input, s1 /= s2 ]

input = [
        L (N 5,L (N 5,N 0)),
        L (L (L (L (N 8,N 4),N 3),L (N 6,N 7)),L (L (L (N 7,N 4),N 2),L (L (N 5,N 7),L (N 1,N 5)))),
        L (L (L (L (N 7,N 6),L (N 4,N 6)),N 1),L (L (N 5,N 7),L (N 0,L (N 2,N 1)))),
        L (L (L (L (N 5,N 8),L (N 3,N 5)),L (N 9,N 0)),L (L (L (N 4,N 0),L (N 4,N 1)),L (L (N 9,N 3),N 1))),
        L (N 7,L (L (L (N 6,N 4),L (N 2,N 7)),L (L (N 8,N 8),N 5))),
        L (L (N 9,N 6),L (N 1,L (L (N 4,N 5),N 3))),
        L (L (L (N 7,N 1),L (N 2,N 4)),L (L (L (N 6,N 2),L (N 1,N 6)),L (N 8,N 8))),
        L (L (L (L (N 1,N 9),L (N 1,N 1)),L (N 2,N 9)),N 5),
        L (N 7,L (L (L (N 0,N 0),N 8),L (L (N 5,N 2),L (N 7,N 6)))),
        L (L (L (N 8,L (N 8,N 7)),N 6),L (N 7,L (N 6,N 2))),
        L (L (N 3,L (N 1,N 3)),L (N 9,L (L (N 0,N 9),N 0))),
        L (L (N 4,L (N 9,L (N 0,N 3))),L (L (N 9,L (N 4,N 6)),N 4)),
        L (L (L (N 6,L (N 5,N 9)),N 1),L (L (N 0,L (N 0,N 4)),L (L (N 1,N 2),N 7))),
        L (L (L (N 8,L (N 1,N 2)),N 0),N 6),
        L (L (L (L (N 1,N 2),N 2),L (L (N 8,N 9),N 9)),L (L (N 4,N 6),L (L (N 7,N 9),L (N 0,N 0)))),
        L (L (L (L (N 1,N 3),L (N 0,N 3)),L (N 2,L (N 8,N 6))),L (L (N 3,L (N 5,N 3)),N 6)),
        L (L (N 7,N 7),L (L (L (N 1,N 1),L (N 5,N 6)),L (L (N 2,N 8),L (N 8,N 7)))),
        L (N 8,L (N 6,L (N 9,L (N 2,N 1)))),
        L (N 9,L (N 2,L (N 9,N 9))),
        L (L (N 6,L (L (N 5,N 0),N 9)),L (N 7,L (L (N 1,N 9),N 3))),
        L (L (N 4,L (N 8,L (N 7,N 8))),L (L (N 4,L (N 4,N 0)),L (L (N 9,N 8),N 3))),
        L (N 5,L (L (N 2,N 3),L (N 7,N 4))),
        L (L (L (L (N 3,N 9),N 1),L (N 8,N 4)),L (L (N 7,N 5),L (L (N 1,N 4),L (N 6,N 4)))),
        L (L (N 5,L (N 7,N 0)),L (N 5,N 8)),
        L (L (L (L (N 8,N 5),L (N 8,N 7)),N 0),L (L (N 7,N 4),N 6)),
        L (L (N 4,L (N 4,L (N 8,N 7))),L (N 9,L (N 7,N 7))),
        L (L (N 4,L (L (N 2,N 9),N 8)),L (N 2,L (N 1,N 6))),
        L (L (L (N 6,L (N 5,N 5)),L (N 3,N 9)),L (N 2,L (L (N 6,N 3),L (N 2,N 5)))),
        L (L (N 2,L (N 7,N 2)),L (L (L (N 9,N 3),N 6),L (N 8,N 4))),
        L (L (L (L (N 0,N 2),L (N 7,N 1)),L (L (N 6,N 4),N 5)),L (L (N 5,L (N 5,N 2)),L (L (N 0,N 4),L (N 5,N 9)))),
        L (L (L (N 6,N 3),L (L (N 3,N 0),N 9)),L (L (L (N 2,N 7),L (N 1,N 1)),L (L (N 4,N 5),L (N 0,N 1)))),
        L (L (L (N 4,L (N 9,N 4)),L (L (N 7,N 3),L (N 6,N 4))),N 5),
        L (L (N 4,L (N 9,N 8)),N 9),
        L (N 7,L (L (L (N 1,N 1),N 3),L (L (N 0,N 2),N 9))),
        L (L (L (L (N 7,N 1),N 6),L (L (N 0,N 4),L (N 5,N 9))),L (N 3,L (L (N 0,N 2),L (N 1,N 9)))),
        L (L (L (N 0,N 6),L (N 1,L (N 4,N 2))),L (N 9,N 7)),
        L (L (L (N 8,L (N 9,N 7)),N 0),L (L (L (N 3,N 4),N 1),N 8)),
        L (L (L (N 7,N 0),L (N 0,N 4)),L (L (N 2,N 1),L (L (N 6,N 7),N 8))),
        L (L (N 2,N 7),L (L (N 6,N 9),L (L (N 0,N 1),L (N 7,N 3)))),
        L (L (L (L (N 0,N 0),L (N 3,N 2)),N 4),L (L (N 6,N 2),N 1)),
        L (L (L (L (N 4,N 6),N 2),L (L (N 5,N 3),N 1)),N 1),
        L (L (L (N 2,L (N 3,N 5)),L (N 5,L (N 4,N 5))),L (L (L (N 2,N 9),N 0),L (N 1,N 2))),
        L (L (L (N 3,N 8),L (N 7,L (N 4,N 5))),N 4),
        L (L (L (N 9,L (N 5,N 2)),L (L (N 3,N 8),N 9)),L (L (N 6,N 6),N 8)),
        L (L (L (L (N 9,N 1),L (N 6,N 7)),L (L (N 0,N 5),N 1)),L (L (N 7,L (N 3,N 0)),N 0)),
        L (N 4,L (L (L (N 6,N 8),N 2),L (L (N 3,N 5),N 5))),
        L (L (L (L (N 1,N 0),N 7),L (N 7,L (N 1,N 2))),L (L (N 6,N 8),L (L (N 4,N 2),L (N 3,N 3)))),
        L (L (N 5,L (N 7,L (N 2,N 5))),N 2),
        L (L (L (L (N 6,N 9),N 0),L (L (N 7,N 2),L (N 3,N 3))),L (N 3,L (L (N 4,N 9),N 3))),
        L (L (N 4,L (N 0,N 8)),L (N 6,N 2)),
        L (L (N 6,L (N 5,N 6)),L (L (L (N 4,N 1),N 7),N 5)),
        L (L (N 6,L (N 2,L (N 1,N 5))),L (L (L (N 0,N 5),N 0),L (N 7,L (N 1,N 2)))),
        L (L (L (L (N 9,N 7),N 9),L (N 4,N 2)),L (N 6,L (L (N 8,N 6),L (N 6,N 0)))),
        L (N 3,L (L (N 2,L (N 7,N 2)),L (N 1,N 9))),
        L (L (N 6,N 9),L (L (N 3,N 0),N 7)),
        L (L (L (L (N 5,N 2),L (N 8,N 8)),L (L (N 8,N 1),N 6)),L (L (L (N 9,N 0),N 2),L (N 4,N 5))),
        L (L (L (N 7,N 0),L (L (N 6,N 8),L (N 9,N 1))),L (N 1,L (N 1,L (N 6,N 0)))),
        L (L (L (N 2,L (N 8,N 2)),L (L (N 1,N 4),L (N 7,N 8))),N 8),
        L (L (L (L (N 7,N 8),N 7),N 7),L (L (L (N 6,N 4),N 0),L (N 8,N 3))),
        L (L (N 6,N 2),L (N 8,N 8)),
        L (L (L (N 8,L (N 2,N 0)),N 7),L (N 7,L (L (N 4,N 3),L (N 9,N 8)))),
        L (L (L (L (N 8,N 5),L (N 2,N 5)),N 4),L (L (L (N 9,N 2),L (N 5,N 1)),L (L (N 0,N 5),L (N 0,N 6)))),
        L (L (L (N 0,L (N 9,N 9)),N 4),L (L (L (N 3,N 8),N 6),L (N 3,L (N 6,N 7)))),
        L (L (L (N 7,N 4),L (N 8,N 7)),L (L (L (N 1,N 1),N 4),L (L (N 3,N 6),L (N 1,N 2)))),
        L (L (L (L (N 0,N 3),L (N 8,N 6)),L (N 8,N 3)),L (N 4,L (N 1,L (N 4,N 7)))),
        L (L (L (N 8,N 2),L (N 6,L (N 2,N 1))),L (L (N 1,L (N 8,N 4)),L (N 3,L (N 3,N 2)))),
        L (L (L (L (N 7,N 4),N 2),L (N 2,L (N 7,N 9))),L (L (L (N 7,N 9),N 6),L (N 2,N 1))),
        L (L (L (N 5,L (N 7,N 4)),L (N 1,L (N 6,N 2))),L (L (N 1,L (N 8,N 9)),L (L (N 2,N 8),N 5))),
        L (L (N 5,N 4),L (L (L (N 6,N 9),N 0),L (L (N 1,N 2),N 9))),
        L (N 0,L (N 0,N 0)),
        L (N 4,L (L (L (N 2,N 3),N 9),L (L (N 0,N 5),L (N 8,N 0)))),
        L (L (L (N 9,N 6),L (N 3,N 0)),L (N 9,L (N 0,N 4))),
        L (L (L (N 5,L (N 4,N 0)),N 8),L (L (N 3,L (N 6,N 1)),L (N 7,L (N 5,N 7)))),
        L (L (L (L (N 2,N 8),L (N 0,N 1)),L (N 9,L (N 8,N 0))),L (L (L (N 2,N 8),L (N 2,N 0)),N 1)),
        L (L (L (L (N 0,N 7),N 5),L (L (N 3,N 3),L (N 0,N 3))),L (L (N 8,L (N 0,N 6)),L (L (N 4,N 3),N 2))),
        L (L (L (L (N 5,N 0),L (N 2,N 2)),N 2),L (L (N 7,L (N 6,N 4)),L (N 6,L (N 6,N 4)))),
        L (L (L (L (N 2,N 7),L (N 0,N 0)),N 8),L (L (L (N 3,N 0),N 2),L (N 0,N 7))),
        L (L (L (N 5,L (N 7,N 6)),L (N 1,L (N 6,N 8))),L (L (N 5,L (N 9,N 3)),L (L (N 8,N 1),N 4))),
        L (L (L (L (N 1,N 1),L (N 0,N 5)),L (L (N 7,N 6),N 5)),L (L (N 7,N 4),L (N 4,L (N 3,N 6)))),
        L (L (L (N 4,L (N 1,N 6)),L (N 4,N 0)),L (L (L (N 8,N 9),L (N 4,N 6)),L (N 9,L (N 4,N 9)))),
        L (L (L (L (N 7,N 7),L (N 8,N 5)),L (N 4,L (N 2,N 2))),L (N 0,L (N 8,L (N 0,N 4)))),
        L (L (L (N 2,N 1),N 9),L (N 5,L (L (N 8,N 9),N 8))),
        L (L (L (N 0,N 7),L (N 9,L (N 0,N 9))),L (N 3,N 3)),
        L (L (N 8,N 7),L (L (N 8,N 7),N 2)),
        L (N 1,L (L (N 9,N 7),N 9)),
        L (L (L (L (N 3,N 4),L (N 7,N 3)),N 3),L (L (N 2,L (N 8,N 2)),L (L (N 8,N 1),L (N 6,N 3)))),
        L (L (N 5,N 7),L (N 0,L (L (N 3,N 4),L (N 0,N 3)))),
        L (L (L (L (N 3,N 3),N 2),L (N 4,N 2)),L (L (N 7,L (N 2,N 4)),L (N 5,N 7))),
        L (L (L (L (N 9,N 4),L (N 9,N 5)),N 1),L (L (N 6,N 7),L (N 6,N 6))),
        L (N 8,L (N 3,N 5)),
        L (L (L (N 3,N 0),N 8),L (N 2,L (N 3,N 7))),
        L (N 3,L (L (L (N 7,N 0),N 5),L (L (N 2,N 2),L (N 8,N 9)))),
        L (L (N 9,L (N 4,N 6)),L (L (L (N 3,N 6),N 8),L (N 5,N 0))),
        L (L (L (N 9,N 5),L (L (N 8,N 7),L (N 0,N 0))),L (L (N 8,L (N 8,N 9)),L (N 6,L (N 8,N 1)))),
        L (L (L (N 4,N 5),L (L (N 1,N 7),L (N 9,N 1))),L (L (L (N 5,N 7),L (N 8,N 9)),L (L (N 4,N 8),N 5))),
        L (L (L (L (N 0,N 7),N 8),L (N 8,N 0)),L (N 2,L (L (N 5,N 4),N 2))),
        L (L (L (N 8,N 4),N 6),L (N 3,L (L (N 0,N 7),L (N 8,N 0)))),
        L (L (L (L (N 1,N 4),L (N 3,N 7)),L (L (N 6,N 9),N 0)),L (N 5,N 5)),
        L (L (N 3,L (L (N 6,N 1),L (N 8,N 7))),L (L (N 7,N 8),N 9)),
        L (L (L (L (N 8,N 5),L (N 7,N 5)),L (L (N 1,N 0),L (N 2,N 7))),L (N 1,L (L (N 2,N 3),L (N 2,N 8))))
    ]
