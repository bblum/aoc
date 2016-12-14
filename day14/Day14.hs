{-# LANGUAGE FlexibleContexts #-}
import qualified Data.Map as M
import Crypto.Hash.MD5
import Data.ByteString.Char8 (pack, unpack)
import Data.ByteString.Base16 (encode)
import Data.List
import Data.Maybe
import Control.Monad.State
import Control.Arrow
import Control.Monad.Trans.Maybe

gethash stretch salt =
    do h0 <- M.lookup (salt, stretch) <$> get
       maybe (let h = unpack $ iterate (encode . hash) (pack $ "ngcjuoqr" ++ show salt) !! stretch
              in do modify $ M.insert (salt, stretch) h; return h)
             return h0

solve salt stretch =
    do triple <- MaybeT $ find ((>2) . length) <$> group <$> gethash stretch salt
       let witness h = any (isPrefixOf triple) $ filter ((>4) . length) $ group h
       MaybeT $ find witness <$> mapM (gethash stretch . (+ salt)) [1..1000]
       return salt

solve2 salt =
    do k1 <- runMaybeT $ solve salt 1
       k2 <- runMaybeT $ solve salt 2017
       return (k1, k2)

main = print $ join (***) ((!! 63) . catMaybes) $ unzip $ evalState (mapM solve2 [0..]) M.empty
