module Foreign.Utilities (repeatIO, Wrapper, newForeignPtr') where

import Control.Exception
import Control.Monad.Cont
import System.IO.Unsafe
import Foreign
import Foreign.Concurrent

repeatIO :: IO a -> IO [a]
repeatIO m = go where go = unsafeInterleaveIO $ liftM2 (:) m go

type Wrapper a = a -> IO (FunPtr a)

newForeignPtr' :: IO (Ptr a) -> (Ptr a -> IO ()) -> IO (ForeignPtr a)
newForeignPtr' new free = do
    p <- new
    Foreign.Concurrent.newForeignPtr p $ free p
