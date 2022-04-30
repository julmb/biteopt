module Main where

import Data.List
import Data.Word
import System.Mem
import System.Random.SplitMix
import Numeric.Biteopt

rosenbrock :: Num a => [a] -> a
rosenbrock xy = (a - x) ^ 2 + b * (y - x ^ 2) ^ 2 where [x, y] = xy; a = 1; b = 100

rng :: Word64 -> [Word32]
rng = unfoldr (Just . nextWord32) . mkSMGen

get :: IO [[Double]]
get = do
    result <- minimize' (Just $ rng 0) [(-2, 2), (-2, 2)] rosenbrock
    return $ take 320 result

main :: IO ()
main = do
    result <- get
    putStrLn $ unlines $ show <$> result
    performGC
    performGC
    putStrLn $ unlines $ show <$> take 2 result
    --print result
