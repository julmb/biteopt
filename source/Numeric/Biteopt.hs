module Numeric.Biteopt (minimize) where

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

rng :: [Word32] -> IO Rng
rng source = const . coerce . pop <$> newIORef source

data Rnd
foreign import ccall "rnd_new" rndNew :: IO (Ptr Rnd)
foreign import ccall "rnd_free" rndFree :: Ptr Rnd -> IO ()
foreign import ccall "rnd_init" rndInit :: Ptr Rnd -> CInt -> FunPtr Rng -> Ptr Void -> IO ()

rnd :: Either Int [Word32] -> IO (ForeignPtr Rnd)
rnd (Left seed) = do
    pr <- manage rndNew rndFree $ return ()
    withForeignPtr pr $ \ pr -> rndInit pr (fromIntegral seed) nullFunPtr nullPtr
    return pr
rnd (Right source) = do
    prf <- rng source >>= rngWrapper
    pr <- manage rndNew rndFree $ trace "rng_free" $ freeHaskellFunPtr prf
    withForeignPtr pr $ \ pr -> rndInit pr 0 prf nullPtr
    return pr

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
foreign import ccall "opt_best" optBest :: Ptr Opt -> Ptr CDouble -> IO ()

opt :: ForeignPtr Rnd -> [(Double, Double)] -> ([Double] -> Double) -> IO (ForeignPtr Opt)
opt pr bounds objective = flip runContT return $ do
    let n = fromIntegral $ length bounds
    po <- lift $ objWrapper $ obj objective
    let (boundLower, boundUpper) = coerce $ unzip bounds
    pbl <- ContT $ withArray boundLower
    pbu <- ContT $ withArray boundUpper
    pm <- lift $ manage optNew optFree $ trace "obj_free" $ freeHaskellFunPtr po
    lift $ withForeignPtr pm $ \ pm -> optSet pm n po nullPtr pbl pbu
    lift $ withForeignPtr pm $ \ pm -> optDims pm n 1
    lift $ withForeignPtr pm $ \ pm -> withForeignPtr pr $ \ pr -> optInit pm pr
    return pm

best :: Int -> ForeignPtr Opt -> IO [Double]
best n pm = flip runContT return $ do
    px <- ContT $ allocaArray n
    lift $ withForeignPtr pm $ \ pm -> optBest pm px
    lift $ coerce $ peekArray n px

get :: Int -> ForeignPtr Opt -> ForeignPtr Rnd -> IO [Double]
get n pm pr = do
    withForeignPtr pm $ \ pm -> withForeignPtr pr $ \ pr -> optStep pm pr
    best n pm

minimize :: Either Int [Word32] -> [(Double, Double)] -> ([Double] -> Double) -> [[Double]]
minimize gen bounds objective = unsafePerformIO $ do
    pr <- rnd gen
    pm <- opt pr bounds objective
    x <- best (length bounds) pm
    xs <- repeatIO $ get (length bounds) pm pr
    return $ x : xs
