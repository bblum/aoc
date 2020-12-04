import Data.List.Split

count f = length . filter f

parse input = map (map parseline) $ splitOn [""] input
    where parseline line = let [a,b] = splitOn ":" line in (a,b)

keys = ["byr","iyr","eyr","hgt","hcl","ecl","pid"]

haskey l k = maybe False (const True) $ lookup k l

validkey l k = maybe False (check k) $ lookup k l
    where between v0 lower upper = let v = read v0 in v >= lower && v <= upper
          check "byr" v0 = between v0 1920 2002
          check "iyr" v0 = between v0 2010 2020
          check "eyr" v0 = between v0 2020 2030
          check "hgt" [x,y,z,'c','m'] = between [x,y,z] 150 193
          check "hgt" [x,y,'i','n'] = between [x,y] 59 76
          check "hgt" _ = False
          check "hcl" ['#',a,b,c,d,e,f] = all (flip elem "0123456789abcdef") [a,b,c,d,e,f]
          check "hcl" _ = False
          check "ecl" v0 = elem v0 ["amb","blu","brn","gry","grn","hzl","oth"]
          check "pid" v0 = length v0 == 9 && all (flip elem "0123456789") v0

valid1 l = all (haskey l) keys
valid2 l = all (validkey l) keys

main = do passports <- parse <$> lines <$> readFile "input.txt"
          print $ count valid1 passports
          print $ count valid2 passports
