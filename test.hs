module Main where

import Numeric.Biteopt
import Foreign.C
import Foreign

test :: Num a => [a] -> a
test xy = (a - x) ^ 2 + b * (y - x ^ 2) ^ 2 where [x, y] = xy; a = 1; b = 100

test' :: Objective
test' n x d = do
    xs <- peekArray (fromIntegral n) x
    return $ test xs

lb :: [CDouble]
lb = [-2, -2]
ub :: [CDouble]
ub = [2, 2]

main :: IO ()
main = do
    result <- minimize [(-2, 2), (-2, 2)] test
    print result
