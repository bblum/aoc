import Text.JSON

getsum (JSRational _ r) = fromRational r
getsum (JSArray js) = sum $ map getsum js
getsum (JSObject o) =
    if any ((== (JSString $ toJSString "red")) . snd) $ fromJSObject o then 0
    else sum $ map (getsum . snd) $ fromJSObject o
getsum _ = 0

main = interact $ (++"\n") . show . (\(Ok j) -> getsum j) . decode
