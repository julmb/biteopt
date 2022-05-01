module Foreign.Utilities (inf, Wrapper, newForeignPtr') where

import Control.Exception
import Control.Monad.Cont
import System.IO.Unsafe
import Foreign
import Foreign.Concurrent

inf :: IO a -> IO [a]
inf m = go where go = unsafeInterleaveIO $ liftM2 (:) m go

type Wrapper a = a -> IO (FunPtr a)

newForeignPtr' :: IO (Ptr a) -> (Ptr a -> IO ()) -> IO (ForeignPtr a)
newForeignPtr' new free = do
    p <- new
    Foreign.Concurrent.newForeignPtr p $ free p
