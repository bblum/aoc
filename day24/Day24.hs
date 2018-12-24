{-# LANGUAGE MonadComprehensions #-}
import Data.List
import Data.Maybe
import Data.Ord

data Group = G { name :: Int, noobs :: Int, hp :: Int, dmg :: Int, dmgtype :: String,
                 initiative :: Int, weaks :: [String], immuns :: [String] } deriving Eq

ep g = noobs g * dmg g
damage g e | elem (dmgtype g) (weaks e) = ep g * 2
damage g e | elem (dmgtype g) (immuns e) = 0
damage g e = ep g

instance Ord Group where
    g1 <= g2 = (ep g1, initiative g1) <= (ep g2, initiative g2)

parse (x,(n:hp:dmg:tp:i:rest)) = G x (read n) (read hp) (read dmg) tp (read i) (weaknesses rest) (immunities rest)
    where weaknesses = takeWhile (/= "immune") . drop 1 . dropWhile (/= "weak")
          immunities = takeWhile (/= "weak")   . drop 1 . dropWhile (/= "immune")

target ([],targets) g = ([],                  ((name g, initiative g), Nothing):targets)
target (es,targets) g = (es \\ maybeToList t, ((name g, initiative g), name <$> t):targets)
    where mb (e,sc) = [ e | sc /= 0 ] -- don't attack if can't hurt anyone
          t = mb $ maximumBy (comparing snd) $ sortBy (comparing fst) $ zip es $ map (damage g) es

attack (is@[immune,infect]) (isInfvec, ((attackerName, attackerInit), Nothing)) = is
attack (is@[immune,infect]) (isInfvec, ((attackerName, attackerInit), Just targetName)) =
    case find ((== attackerName) . name) (is !! isInfvec) of
        Nothing -> is -- group dekilled already, do nothing
        Just attacker -> [[immune, mapMaybe fiteMe infect],[mapMaybe fiteMe immune, infect]] !! isInfvec
            where Just oppo = find ((== targetName) . name) (is !! (1-isInfvec))
                  unitsLost = div (damage attacker oppo) (hp oppo)
                  fiteMe g | name g == targetName && noobs g <= unitsLost = Nothing -- dies
                  fiteMe g | name g == targetName = Just $ g { noobs = noobs g - unitsLost }
                  fiteMe g = Just g

fight [immune,infect] = foldl attack [immune,infect] $ reverse $ sortBy (comparing $ snd . fst . snd) $ zip (repeat 0) immunetargets ++ zip (repeat 1) infecttargets
    where immunetargets = snd $ foldl target (infect,[]) $ reverse $ sort immune
          infecttargets = snd $ foldl target (immune,[]) $ reverse $ sort infect

main = do immune <- map parse <$> zip [1..] <$> map words <$> lines <$> readFile "immune.txt"
          infect <- map parse <$> zip [1..] <$> map words <$> lines <$> readFile "infect.txt"
          print $ sum $ map noobs $ last $ until (any null) fight [immune,infect]
          print $ sum $ map noobs $ head $ until (any null) fight [map (\g -> g { dmg = dmg g + 34 }) immune, infect]
