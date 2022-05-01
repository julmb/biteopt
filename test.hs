module Main where

import Control.Concurrent
import Control.Parallel.Strategies
import Data.List
import Data.Word
import System.Mem
import System.Random.SplitMix
import Numeric.Biteopt

rng :: Word64 -> [Word32]
rng = unfoldr (Just . nextWord32) . mkSMGen

rosenbrock :: Num a => [a] -> a
rosenbrock xy = (a - x) ^ 2 + b * (y - x ^ 2) ^ 2 where [x, y] = xy; a = 1; b = 100

slow :: (Num a, Enum a) => [a] -> a
slow xs = sum $ map (x *) [0..1000000] where [x] = xs

resultRosenbrock :: [[Double]]
resultRosenbrock = take 320 $ minimize (Right $ rng 0) [(-2, 2), (-2, 2)] rosenbrock

resultSlow :: Word64 -> [[Double]]
resultSlow i = take 1000 $ minimize (Right $ rng i) [(-1, 1)] slow

testRosenbrock :: IO ()
testRosenbrock = do
    putStrLn $ unlines $ show <$> take 10 resultRosenbrock
    performGC
    threadDelay 1000
    putStrLn $ unlines $ show <$> drop 310 resultRosenbrock
    performGC
    threadDelay 1000
    putStrLn $ unlines $ show <$> take 10 resultRosenbrock

testSlow :: IO ()
testSlow = do
    getNumCapabilities >>= print
    let a = last . resultSlow <$> [0..3]
    let b = withStrategy $ parList rseq
    print $ b a
    performGC
    threadDelay 1000
    print $ b a

main :: IO ()
main = do
    --testRosenbrock
    testSlow
