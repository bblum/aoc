viable a b used avail = a /= b && read used /= 0 && read avail - read used >= 0

viables input = [ (a,b) | a:_:used:_ <- input,
                          b:_:_:avail:_ <- input,
                          viable a b used avail ]

main = interact $ show . length . viables . map words . lines
