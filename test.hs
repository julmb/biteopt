module Main where

import Numeric.Biteopt

rosenbrock :: Num a => [a] -> a
rosenbrock xy = (a - x) ^ 2 + b * (y - x ^ 2) ^ 2 where [x, y] = xy; a = 1; b = 100

main :: IO ()
main = minimize [(-2, 2), (-2, 2)] rosenbrock >>= print
