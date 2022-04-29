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
