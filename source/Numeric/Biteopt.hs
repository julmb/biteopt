module Numeric.Biteopt (minimize) where

import Control.Monad.Cont
import Data.Void
import Data.Coerce
import Data.IORef
import System.IO.Unsafe
import Foreign
import Foreign.C
import Foreign.Utilities
import Debug.Trace

type Rng = Ptr Void -> IO CUInt
foreign import ccall "wrapper" rngWrapper :: Wrapper Rng

rng :: [Word32] -> IO Rng
rng source = const . coerce . pop <$> newIORef source

data Rnd
foreign import ccall "rnd_new" rndNew :: IO (Ptr Rnd)
foreign import ccall "rnd_free" rndFree :: Ptr Rnd -> IO ()
foreign import ccall "rnd_init" rndInit :: Ptr Rnd -> CInt -> FunPtr Rng -> Ptr Void -> IO ()

rnd :: Either Int [Word32] -> IO (ForeignPtr Rnd)
rnd (Left seed) = do
    prnd <- manage rndNew rndFree $ return ()
    withForeignPtr prnd $ \ prnd -> rndInit prnd (fromIntegral seed) nullFunPtr nullPtr
    return prnd
rnd (Right source) = do
    prng <- rng source >>= rngWrapper
    prnd <- manage rndNew rndFree $ trace "rng_free" $ freeHaskellFunPtr prng
    withForeignPtr prnd $ \ prnd -> rndInit prnd 0 prng nullPtr
    return prnd

type Obj = CInt -> Ptr CDouble -> Ptr Void -> IO CDouble
foreign import ccall "wrapper" objWrapper :: Wrapper Obj

obj :: ([Double] -> Double) -> Obj
obj f n p = const $ coerce f <$> peekArray (fromIntegral n) p

data Opt
foreign import ccall "opt_new" optNew :: IO (Ptr Opt)
foreign import ccall "opt_free" optFree :: Ptr Opt -> IO ()
foreign import ccall "opt_set" optSet :: Ptr Opt -> CInt -> FunPtr Obj -> Ptr Void -> Ptr CDouble -> Ptr CDouble -> IO ()
foreign import ccall "opt_dims" optDims :: Ptr Opt -> CInt -> CInt -> IO ()
foreign import ccall "opt_init" optInit :: Ptr Opt -> Ptr Rnd -> IO ()
foreign import ccall "opt_step" optStep :: Ptr Opt -> Ptr Rnd -> IO CInt
foreign import ccall "opt_best" optBest :: Ptr Opt -> IO (Ptr CDouble)

opt :: [(Double, Double)] -> ([Double] -> Double) -> ForeignPtr Rnd -> IO (ForeignPtr Opt)
opt bounds objective prnd = flip runContT return $ do
    let n = fromIntegral $ length bounds
    pobj <- lift $ objWrapper $ obj objective
    let (boundLower, boundUpper) = unzip bounds
    pbl <- ContT $ withArray $ coerce boundLower
    pbu <- ContT $ withArray $ coerce boundUpper
    popt <- lift $ manage optNew optFree $ trace "obj_free" $ freeHaskellFunPtr pobj
    lift $ withForeignPtr popt $ \ popt -> optSet popt n pobj nullPtr pbl pbu
    lift $ withForeignPtr popt $ \ popt -> optDims popt n 1
    lift $ withForeignPtr popt $ \ popt -> withForeignPtr prnd $ \ prnd -> optInit popt prnd
    return popt

step :: ForeignPtr Opt -> ForeignPtr Rnd -> Int -> IO [Double]
step popt prnd n = do
    withForeignPtr popt $ withForeignPtr prnd . optStep
    coerce $ withForeignPtr popt optBest >>= peekArray n

minimize :: Either Int [Word32] -> [(Double, Double)] -> ([Double] -> Double) -> [[Double]]
minimize gen bounds objective = unsafePerformIO $ do
    prnd <- rnd gen
    popt <- opt bounds objective prnd
    repeatIO $ step popt prnd $ length bounds
