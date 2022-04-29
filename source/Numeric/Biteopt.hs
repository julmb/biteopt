module Numeric.Biteopt where

import Data.Coerce
import Foreign
import Foreign.C

type Objective = CInt -> Ptr CDouble -> Ptr () -> IO CDouble
type RNG = Ptr () -> IO CUInt

foreign import ccall "wrapper" mkObjective :: Objective -> IO (FunPtr Objective)
foreign import ccall "minimize" boMinimize ::
    CInt -> FunPtr Objective -> Ptr () ->
    Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
    CInt -> CInt -> CInt ->
    CInt -> FunPtr RNG -> Ptr () -> IO CInt

oo :: ([Double] -> Double) -> Objective
oo objective n x d = do
    xs <- peekArray (fromIntegral n) x
    return $ coerce $ objective $ coerce xs

minimize :: [(Double, Double)] -> ([Double] -> Double) -> IO ([Double], Double, CInt)
minimize bounds objective = do
    let dimensions = length bounds
    obj <- mkObjective $ oo objective
    let (lbl, ubl) = unzip $ coerce bounds
    (c, x, fx) <- allocaArray dimensions $ \ x ->
        alloca $ \ fx ->
            withArray lbl $ \ lba ->
                withArray ubl $ \ uba -> do
                    c <- boMinimize (fromIntegral dimensions) obj nullPtr lba uba x fx 20000 1 1 1 nullFunPtr nullPtr
                    xs <- peekArray dimensions x
                    a <- peek fx
                    return (c, xs, a)
    freeHaskellFunPtr obj
    return (coerce x, coerce fx, c)
