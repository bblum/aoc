import Data.Maybe

xmin = 117
xmax = 164
ymin = -140
ymax = -89

step ((xpos,xv),(ypos,yv)) = ((xpos+xv, max 0 $ xv-1),(ypos+yv,yv-1))

toofar ((xpos,_),(ypos,_)) = ypos < ymin || xpos > xmax
go xv yv = takeWhile (not . toofar) $ scanl (\b a -> step b) ((0,xv),(0,yv)) [0..]


part1 = maximum $ catMaybes $ map try [1..1000]
    where try yv = if good1 $ last $ go 0 yv then Just $ maximum $ map (fst . snd) $ go 0 yv else Nothing
          good1 (_,(ypos,_)) = elem ypos [ymin..ymax]


part2 = length $ filter id $ map try [-200..200] <*> [-200..200]
    where try xv yv = good2 $ last $ go xv yv
          good2 ((xpos,_),(ypos,_)) = elem ypos [ymin..ymax] && elem xpos [xmin..xmax]

main = do print part1; print part2
