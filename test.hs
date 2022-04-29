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
    putStrLn "test"
    objective <- mkObjective test'
    let n = 2
    allocaArray n $ \ x ->
        alloca $ \ fx ->
            withArray lb $ \ lba ->
                withArray ub $ \ uba -> do
                    c <- minimize (fromIntegral n) objective nullPtr lba uba x fx 20000 1 1 1 nullFunPtr nullPtr
                    print c
                    xs <- peekArray n x
                    print xs
                    a <- peek fx
                    print a
    freeHaskellFunPtr objective
