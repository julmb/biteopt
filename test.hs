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

runRosenbrock :: Word64 -> [[Double]]
runRosenbrock i = take 320 $ minimize (Right $ rng i) [(-2, 2), (-2, 2)] rosenbrock

runSlow :: Int -> [[Double]]
runSlow i = take 1000 $ minimize (Left i) [(-1, 1)] slow

testRosenbrock :: IO ()
testRosenbrock = do
    let result = runRosenbrock 0
    putStrLn $ unlines $ show <$> take 10 result
    performGC
    threadDelay 1000
    putStrLn $ unlines $ show <$> drop 310 result
    performGC
    threadDelay 1000
    putStrLn $ unlines $ show <$> take 10 result

testRosenbrockMany :: IO ()
testRosenbrockMany = do
    let par = withStrategy $ parList rseq
    let result = par $ rosenbrock . last . runRosenbrock <$> [0..10]
    print result

testSlow :: IO ()
testSlow = do
    let par = withStrategy $ parList rseq
    let result = par $ last . runSlow <$> [0..3]
    print result
    performGC
    threadDelay 1000
    print result

main :: IO ()
main = do
    getNumCapabilities >>= print
    --testRosenbrock
    --testRosenbrockMany
    testSlow
