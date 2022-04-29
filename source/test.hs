module Main where

import Foreign
import Foreign.C

type Objective = CInt -> Ptr CDouble -> Ptr () -> IO CDouble
type RNG = Ptr () -> IO CUInt

foreign import ccall "wrapper" mkObjective :: Objective -> IO (FunPtr Objective)
foreign import ccall "minimize" minimize ::
    CInt -> FunPtr Objective -> Ptr () ->
    Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
    CInt -> CInt -> CInt ->
    CInt -> FunPtr RNG -> Ptr () -> IO CInt

test :: Num a => [a] -> a
test xy = (x - 1) ^ 2 + 2 * (y + 1) ^ 2 where [x, y] = xy

test' :: Objective
test' n x d = return 0

main :: IO ()
main = do
    putStrLn "test"
    objective <- mkObjective test'

    freeHaskellFunPtr objective

