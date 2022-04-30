{-# LANGUAGE FlexibleContexts #-}

module Numeric.Biteopt (minimize, minimize') where

import Control.Monad
import Control.Monad.Cont
import Data.Void
import Data.Coerce
import Data.IORef
import Foreign
import Foreign.C
import Foreign.Utilities

type BiteObj = CInt -> Ptr CDouble -> Ptr Void -> IO CDouble
foreign import ccall "wrapper" objWrapper :: Wrapper BiteObj

type BiteRnd = Ptr Void -> IO CUInt
foreign import ccall "wrapper" rngWrapper :: Wrapper BiteRnd

foreign import ccall "biteopt_minimize_wrapper" biteoptMinimize ::
    CInt -> FunPtr BiteObj -> Ptr Void ->
    Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
    CInt -> CInt -> CInt ->
    CInt -> FunPtr BiteRnd -> Ptr Void -> IO CInt

data Min
foreign import ccall "minimizer_new" minimizerNew :: CInt -> FunPtr BiteObj -> Ptr Void -> Ptr CDouble -> Ptr CDouble -> CInt -> FunPtr BiteRnd -> IO (Ptr Min)
foreign import ccall "minimizer_free" minimizerFree :: Ptr Min -> IO ()
foreign import ccall "minimizer_step" minimizerStep :: Ptr Min -> IO Int
foreign import ccall "minimizer_best" minimizerBest :: Ptr Min -> Ptr CDouble -> IO ()

biteObj :: ([Double] -> Double) -> ContT r IO (FunPtr BiteObj)
biteObj f = withWrapper objWrapper eval where
    eval n p = const $ do
        xs <- peekArray (fromIntegral n) p
        return $ coerce $ f $ coerce xs

biteRnd :: [Word32] -> ContT r IO (FunPtr BiteRnd)
biteRnd xs = lift (newIORef xs) >>= withWrapper rngWrapper . next where
    next r = const $ do
        x : xs <- readIORef r
        writeIORef r xs
        return $ coerce x

get :: Int -> Ptr Min -> ContT r IO [Double]
get n pm = do
    lift $ minimizerStep pm
    px <- ContT $ allocaArray n
    lift $ minimizerBest pm px
    xs <- lift $ peekArray n px
    return $ coerce xs

minimize' :: Maybe [Word32] -> [(Double, Double)] -> ([Double] -> Double) -> IO [[Double]]
minimize' rng bounds objective = flip runContT return $ do
    let dimensions = length bounds
    po <- biteObj objective
    let (boundLower, boundUpper) = unzip $ coerce bounds
    pbl <- ContT $ withArray boundLower
    pbu <- ContT $ withArray boundUpper
    pr <- maybe (return nullFunPtr) biteRnd rng
    pm <- lift $ minimizerNew (fromIntegral dimensions) po nullPtr pbl pbu 1 pr
    result <- replicateM 100 $ get dimensions pm
    lift $ minimizerFree pm
    return result

minimize :: Maybe [Word32] -> [(Double, Double)] -> ([Double] -> Double) -> IO ([Double], Double, CInt)
minimize rng bounds objective = flip runContT return $ do
    let dimensions = length bounds
    po <- biteObj objective
    let (boundLower, boundUpper) = unzip $ coerce bounds
    pbl <- ContT $ withArray boundLower
    pbu <- ContT $ withArray boundUpper
    px <- ContT $ allocaArray dimensions
    py <- ContT alloca
    pr <- maybe (return nullFunPtr) biteRnd rng
    n <- lift $ biteoptMinimize (fromIntegral dimensions) po nullPtr pbl pbu px py 20000 1 1 1 pr nullPtr
    x <- lift $ peekArray dimensions px
    y <- lift $ peek py
    return (coerce x, coerce y, n)
