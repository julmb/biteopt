module Numeric.Biteopt (minimize) where

import Control.Monad
import Control.Monad.Cont
import Data.Void
import Data.Coerce
import Data.IORef
import Foreign
import Foreign.C
import Foreign.Utilities

type BiteObj = CInt -> Ptr CDouble -> Ptr Void -> IO CDouble
foreign import ccall "wrapper" objectiveWrapper :: Wrapper BiteObj

type BiteRnd = Ptr Void -> IO CUInt
foreign import ccall "wrapper" rngWrapper :: Wrapper BiteRnd

foreign import ccall "biteopt_minimize_wrapper" biteoptMinimize ::
    CInt -> FunPtr BiteObj -> Ptr Void ->
    Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
    CInt -> CInt -> CInt ->
    CInt -> FunPtr BiteRnd -> Ptr Void -> IO CInt

rng :: Maybe [Word32] -> ContT r IO (FunPtr BiteRnd)
rng Nothing = return nullFunPtr
rng (Just xs) = lift (newIORef xs) >>= withWrapper rngWrapper . next

next :: IORef [Word32] -> BiteRnd
next r = const $ do
    x : xs <- readIORef r
    writeIORef r xs
    return $ coerce x

oo :: ([Double] -> Double) -> BiteObj
oo objective n p = const $ do
    xs <- peekArray (fromIntegral n) p
    return $ coerce $ objective $ coerce xs

minimize :: Maybe [Word32] -> [(Double, Double)] -> ([Double] -> Double) -> IO ([Double], Double, CInt)
minimize r bounds objective = flip runContT return $ do
    let dimensions = length bounds
    obj <- withWrapper objectiveWrapper $ oo objective
    let (lbl, ubl) = unzip $ coerce bounds
    lba <- ContT $ withArray lbl
    uba <- ContT $ withArray ubl
    x <- ContT $ allocaArray dimensions
    fx <- ContT alloca
    rf <- rng r
    c <- lift $ biteoptMinimize (fromIntegral dimensions) obj nullPtr lba uba x fx 20000 1 1 1 rf nullPtr
    xs <- lift $ peekArray dimensions x
    a <- lift $ peek fx
    return (coerce xs, coerce a, c)
