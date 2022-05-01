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
import Foreign.Utilities
import System.IO.Unsafe

type BiteRnd = Ptr Void -> IO CUInt
foreign import ccall "wrapper" rngWrapper :: Wrapper BiteRnd

data Rnd
foreign import ccall "rnd_new" rndNew :: IO (Ptr Rnd)
foreign import ccall "&rnd_free" rndFree :: FunPtr (Ptr Rnd -> IO ())
foreign import ccall "rnd_init" rndInit :: Ptr Rnd -> CInt -> FunPtr BiteRnd -> Ptr Void -> IO ()

type BiteObj = CInt -> Ptr CDouble -> Ptr Void -> IO CDouble
foreign import ccall "wrapper" objWrapper :: Wrapper BiteObj

data Opt
foreign import ccall "opt_new" optNew :: IO (Ptr Opt)
foreign import ccall "&opt_free" optFree :: FunPtr (Ptr Opt -> IO ())
foreign import ccall "opt_set" optSet :: Ptr Opt -> CInt -> FunPtr BiteObj -> Ptr Void -> Ptr CDouble -> Ptr CDouble -> IO ()
foreign import ccall "opt_dims" optDims :: Ptr Opt -> CInt -> CInt -> IO ()
foreign import ccall "opt_init" optInit :: Ptr Opt -> Ptr Rnd -> IO ()
foreign import ccall "opt_step" optStep :: Ptr Opt -> Ptr Rnd -> IO CInt
foreign import ccall "opt_best" optBest :: Ptr Opt -> Ptr CDouble -> IO ()

foreign import ccall "biteopt_minimize_wrapper" biteoptMinimize ::
    CInt -> FunPtr BiteObj -> Ptr Void ->
    Ptr CDouble -> Ptr CDouble -> Ptr CDouble -> Ptr CDouble ->
    CInt -> CInt -> CInt ->
    CInt -> FunPtr BiteRnd -> Ptr Void -> IO CInt

biteRnd :: [Word32] -> ContT r IO (FunPtr BiteRnd)
biteRnd xs = lift (newIORef xs) >>= withWrapper . rngWrapper . next where
    next r = const $ do
        x : xs <- readIORef r
        writeIORef r xs
        return $ coerce x

biteObj :: ([Double] -> Double) -> ContT r IO (FunPtr BiteObj)
biteObj f = withWrapper $ objWrapper eval where
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

mkRng :: Maybe [Word32] -> ContT r IO (ForeignPtr Rnd)
mkRng rng = do
    prf <- maybe (return nullFunPtr) biteRnd rng
    pr <- lift $ rndNew >>= newForeignPtr rndFree
    -- TODO: expose seed of integrated rng
    lift $ withForeignPtr pr $ \ pr -> rndInit pr 0 prf nullPtr
    return pr

minimize' :: Maybe [Word32] -> [(Double, Double)] -> ([Double] -> Double) -> [[Double]]
minimize' rng bounds objective = unsafePerformIO $ flip runContT return $ do
    pr <- mkRng rng
    -- TODO: fromIntegral here?
    let dimensions = length bounds
    po <- biteObj objective
    let (boundLower, boundUpper) = unzip $ coerce bounds
    pbl <- ContT $ withArray boundLower
    pbu <- ContT $ withArray boundUpper
    pm <- lift $ optNew >>= newForeignPtr optFree
    lift $ withForeignPtr pm $ \ pm -> optSet pm (fromIntegral dimensions) po nullPtr pbl pbu
    lift $ withForeignPtr pm $ \ pm -> optDims pm (fromIntegral dimensions) 1
    lift $ withForeignPtr pm $ \ pm -> withForeignPtr pr $ \ pr -> optInit pm pr
    lift $ inf $ flip runContT return $ get dimensions pm pr

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
