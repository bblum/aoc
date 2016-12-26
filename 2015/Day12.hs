{-# LANGUAGE FlexibleContexts, TupleSections #-}
import qualified Data.Map as M
import Text.JSON
import Data.List
import Data.Maybe
import Data.Char
import Control.Monad.State
import Control.Arrow
import Debug.Trace

solve1 (JSRational _ r) = fromRational r
solve1 (JSArray a) = sum $ map solve1 a
solve1 (JSObject o) = sum $ map solve1 $ map snd $ fromJSObject o
solve1 _ = 0

solve2 (JSRational _ r) = fromRational r
solve2 (JSArray a) = sum $ map solve2 a
solve2 (JSObject o) = sum $ map solve2 $ noreds $ map snd $ fromJSObject o
    where noreds l = if any (== (JSString $ toJSString "red")) l then [] else l
solve2 _ = 0

main = interact $ (++"\n") . show . (solve1 &&& solve2) . (\(Ok j) -> j) . decode
