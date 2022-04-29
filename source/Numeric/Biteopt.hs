module Numeric.Biteopt (minimize) where

import Control.Monad
import Control.Monad.Cont
import Data.Void
import Data.Coerce
import Data.IORef
import Foreign
import Foreign.C

type Objective = CInt -> Ptr CDouble -> Ptr Void -> IO CDouble
foreign import ccall "wrapper" objectiveWrapper :: Objective -> IO (FunPtr Objective)

type BiteRnd = Ptr Void -> IO CUInt
foreign import ccall "wrapper" rngWrapper :: BiteRnd -> IO (FunPtr BiteRnd)

foreign import ccall "biteopt_minimize_wrapper" biteoptMinimize ::
    CInt -> FunPtr Objective -> Ptr Void ->
    Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
    CInt -> CInt -> CInt ->
    CInt -> FunPtr BiteRnd -> Ptr Void -> IO CInt

withWrapper :: (a -> IO (FunPtr a)) -> a -> ContT r IO (FunPtr a)
withWrapper wrapper f = ContT $ \ action -> do
    p <- wrapper f
    result <- action p
    freeHaskellFunPtr p
    return result

rng :: Maybe [Word32] -> ContT r IO (FunPtr BiteRnd)
rng Nothing = return nullFunPtr
rng (Just xs) = do
            rd <- lift $ newIORef xs
            let f = const $ do
                    x : xs <- readIORef rd
                    writeIORef rd xs
                    return (coerce x :: CUInt)
            withWrapper rngWrapper f

oo :: ([Double] -> Double) -> Objective
oo objective n x d = do
    xs <- peekArray (fromIntegral n) x
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
