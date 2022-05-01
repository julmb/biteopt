module Foreign.Utilities (repeatIO, Wrapper, manage) where

import Control.Monad
import System.IO.Unsafe
import Foreign
import Foreign.Concurrent

repeatIO :: IO a -> IO [a]
repeatIO m = go where go = unsafeInterleaveIO $ liftM2 (:) m go

type Wrapper a = a -> IO (FunPtr a)

manage :: IO (Ptr a) -> (Ptr a -> IO ()) -> IO () -> IO (ForeignPtr a)
manage new free finalize = do p <- new; Foreign.Concurrent.newForeignPtr p $ free p >> finalize
