module Numeric.Biteopt (minimize) where

import Control.Monad
import Data.Void
import Data.Coerce
import Data.IORef
import Foreign
import Foreign.C

type Objective = CInt -> Ptr CDouble -> Ptr Void -> IO CDouble
foreign import ccall "wrapper" objPtr :: Objective -> IO (FunPtr Objective)

type BiteRnd = Ptr Void -> IO CUInt
foreign import ccall "wrapper" rngPtr :: BiteRnd -> IO (FunPtr BiteRnd)

foreign import ccall "biteopt_minimize_c" boMinimize ::
    CInt -> FunPtr Objective -> Ptr Void ->
    Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
    CInt -> CInt -> CInt ->
    CInt -> FunPtr BiteRnd -> Ptr Void -> IO CInt

rng :: Maybe [Word32] -> IO (FunPtr BiteRnd)
rng Nothing = return nullFunPtr
rng (Just xs) = do
            rd <- newIORef xs
            let f = const $ do
                    x : xs <- readIORef rd
                    writeIORef rd xs
                    return (coerce x :: CUInt)
            rngPtr f

oo :: ([Double] -> Double) -> Objective
oo objective n x d = do
    xs <- peekArray (fromIntegral n) x
    return $ coerce $ objective $ coerce xs

minimize :: Maybe [Word32] -> [(Double, Double)] -> ([Double] -> Double) -> IO ([Double], Double, CInt)
minimize r bounds objective = do
    rf <- rng r
    let dimensions = length bounds
    obj <- objPtr $ oo objective
    let (lbl, ubl) = unzip $ coerce bounds
    (c, x, fx) <- allocaArray dimensions $ \ x ->
        alloca $ \ fx ->
            withArray lbl $ \ lba ->
                withArray ubl $ \ uba -> do
                    c <- boMinimize (fromIntegral dimensions) obj nullPtr lba uba x fx 20000 1 1 1 rf nullPtr
                    xs <- peekArray dimensions x
                    a <- peek fx
                    return (c, xs, a)
    when (rf /= nullFunPtr) $ freeHaskellFunPtr rf
    freeHaskellFunPtr obj
    return (coerce x, coerce fx, c)
