{-# LANGUAGE FlexibleContexts #-}

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
foreign import ccall "wrapper" objWrapper :: Wrapper BiteObj

type BiteRnd = Ptr Void -> IO CUInt
foreign import ccall "wrapper" rngWrapper :: Wrapper BiteRnd

foreign import ccall "biteopt_minimize_wrapper" biteoptMinimize ::
    CInt -> FunPtr BiteObj -> Ptr Void ->
    Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
    CInt -> CInt -> CInt ->
    CInt -> FunPtr BiteRnd -> Ptr Void -> IO CInt

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
