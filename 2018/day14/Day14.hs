create (e,f,recipes,l) =
    let er = recipes !! (l-1-e)
        fr = recipes !! (l-1-f)
        newr = if er+fr > 9 then [div (er+fr) 10, mod (er+fr) 10] else [er+fr]
        newrecipes = reverse newr ++ recipes
        newe = mod (e + 1 + er) (length newrecipes)
        newf = mod (f + 1 + fr) (length newrecipes)
        -- part2 = take (length input2) newrecipes == reverse input2
    in -- if part2 then error (show ("p2",length newrecipes)) else
       (newe,newf,newrecipes,l + length newr)

done (_,_,_,l) = l >= input+10
reverse_ (_,_,recipes,_) = reverse recipes
main = print $ map (head . show) $ take 10 $ drop input $ reverse_ $ until done create (0,1,[7,3],2)

input = 110201

-- input2 = [1,1,0,2,0,1]
-- done2 (_,_,recipes,_) = take (length input2) recipes == reverse input2
-- print $ until done2 create (0,1,[7,3],2) -- XXX: this is part 2, but it's fucked
