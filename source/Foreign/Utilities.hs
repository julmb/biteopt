module Foreign.Utilities (repeatIO, Wrapper, newForeignPtr') where

import Control.Exception
import Control.Monad.Cont
import System.IO.Unsafe
import Foreign
import Foreign.Concurrent

repeatIO :: IO a -> IO [a]
repeatIO m = go where go = unsafeInterleaveIO $ liftM2 (:) m go

type Wrapper a = a -> IO (FunPtr a)

newForeignPtr' :: IO (Ptr a) -> (Ptr a -> IO ()) -> IO () -> IO (ForeignPtr a)
newForeignPtr' new free finalize = do
    p <- new
    pf <- newForeignPtr_ p
    Foreign.Concurrent.addForeignPtrFinalizer pf finalize
    Foreign.Concurrent.addForeignPtrFinalizer pf $ free p
    return pf
