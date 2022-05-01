{-# LANGUAGE FlexibleContexts #-}

module Numeric.Biteopt (minimize, minimize') where

import Control.Exception
import Control.Monad
import Control.Monad.Cont
import Data.Void
import Data.Coerce
import Data.IORef
import Foreign
import Foreign.C
import Foreign.Concurrent
import Foreign.Utilities
import System.IO.Unsafe
import Debug.Trace

type Rng = Ptr Void -> IO CUInt
foreign import ccall "wrapper" rngWrapper :: Wrapper Rng

rng :: [Word32] -> IO (FunPtr Rng)
rng xs = newIORef xs >>= rngWrapper . next where
    next r = const $ do
        x : xs <- readIORef r
        writeIORef r xs
        return $ coerce x

data Rnd
foreign import ccall "rnd_new" rndNew :: IO (Ptr Rnd)
foreign import ccall "rnd_free" rndFree :: Ptr Rnd -> IO ()
foreign import ccall "rnd_init" rndInit :: Ptr Rnd -> CInt -> FunPtr Rng -> Ptr Void -> IO ()

rnd :: Maybe [Word32] -> IO (ForeignPtr Rnd)
rnd source = do
    prf <- maybe (return nullFunPtr) rng source
    pr <- manage rndNew rndFree $ trace "rf_free" $ freeHaskellFunPtr prf
    -- TODO: expose seed of integrated rng
    withForeignPtr pr $ \ pr -> rndInit pr 0 prf nullPtr
    return pr

type Obj = CInt -> Ptr CDouble -> Ptr Void -> IO CDouble
foreign import ccall "wrapper" objWrapper :: Wrapper Obj

data Opt
foreign import ccall "opt_new" optNew :: IO (Ptr Opt)
foreign import ccall "opt_free" optFree :: Ptr Opt -> IO ()
foreign import ccall "opt_set" optSet :: Ptr Opt -> CInt -> FunPtr Obj -> Ptr Void -> Ptr CDouble -> Ptr CDouble -> IO ()
foreign import ccall "opt_dims" optDims :: Ptr Opt -> CInt -> CInt -> IO ()
foreign import ccall "opt_init" optInit :: Ptr Opt -> Ptr Rnd -> IO ()
foreign import ccall "opt_step" optStep :: Ptr Opt -> Ptr Rnd -> IO CInt
foreign import ccall "opt_best" optBest :: Ptr Opt -> Ptr CDouble -> IO ()

foreign import ccall "biteopt_minimize_wrapper" biteoptMinimize ::
    CInt -> FunPtr Obj -> Ptr Void ->
    Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
    CInt -> CInt -> CInt ->
    CInt -> FunPtr Rng -> Ptr Void -> IO CInt

biteObj :: ([Double] -> Double) -> IO (FunPtr Obj)
biteObj f = objWrapper eval where
    eval n p = const $ do
        xs <- peekArray (fromIntegral n) p
        return $ coerce $ f $ coerce xs

get :: Int -> ForeignPtr Opt -> ForeignPtr Rnd -> ContT r IO [Double]
get n pm pr = do
    lift $ withForeignPtr pm $ \ pm -> withForeignPtr pr $ \ pr -> optStep pm pr
    px <- ContT $ allocaArray n
    lift $ withForeignPtr pm $ \ pm -> optBest pm px
    xs <- lift $ peekArray n px
    return $ coerce xs

minimize' :: Maybe [Word32] -> [(Double, Double)] -> ([Double] -> Double) -> [[Double]]
minimize' gen bounds objective = unsafePerformIO $ flip runContT return $ do
    pr <- lift $ rnd gen
    -- TODO: fromIntegral here?
    let dimensions = length bounds
    po <- lift $ biteObj objective
    let (boundLower, boundUpper) = unzip $ coerce bounds
    pbl <- ContT $ withArray boundLower
    pbu <- ContT $ withArray boundUpper
    pm <- lift $ manage optNew optFree $ trace "obj_free" $ freeHaskellFunPtr po
    lift $ withForeignPtr pm $ \ pm -> optSet pm (fromIntegral dimensions) po nullPtr pbl pbu
    lift $ withForeignPtr pm $ \ pm -> optDims pm (fromIntegral dimensions) 1
    lift $ withForeignPtr pm $ \ pm -> withForeignPtr pr $ \ pr -> optInit pm pr
    lift $ repeatIO $ flip runContT return $ get dimensions pm pr

minimize :: Maybe [Word32] -> [(Double, Double)] -> ([Double] -> Double) -> IO ([Double], Double, CInt)
minimize gen bounds objective = flip runContT return $ do
    let dimensions = length bounds
    po <- lift $ biteObj objective
    let (boundLower, boundUpper) = unzip $ coerce bounds
    pbl <- ContT $ withArray boundLower
    pbu <- ContT $ withArray boundUpper
    px <- ContT $ allocaArray dimensions
    py <- ContT alloca
    pr <- lift $ maybe (return nullFunPtr) rng gen
    n <- lift $ biteoptMinimize (fromIntegral dimensions) po nullPtr pbl pbu px py 20000 1 1 1 pr nullPtr
    x <- lift $ peekArray dimensions px
    y <- lift $ peek py
    return (coerce x, coerce y, n)
