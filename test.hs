module Main where

import Data.List
import Data.Word
import System.Random.SplitMix
import Numeric.Biteopt

rosenbrock :: Num a => [a] -> a
rosenbrock xy = (a - x) ^ 2 + b * (y - x ^ 2) ^ 2 where [x, y] = xy; a = 1; b = 100

rng :: Word64 -> [Word32]
rng = unfoldr (Just . nextWord32) . mkSMGen

main :: IO ()
main = do
    result <- minimize' (Just $ rng 0) [(-2, 2), (-2, 2)] rosenbrock
    putStrLn $ unlines $ show <$> result
    --print result
