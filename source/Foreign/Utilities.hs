module Foreign.Utilities (repeatIO, push, pop, Wrapper, manage, buffer) where

import Control.Monad
import Data.IORef
import System.IO.Unsafe
import Foreign
import Foreign.Concurrent

repeatIO :: IO a -> IO [a]
repeatIO m = go where go = unsafeInterleaveIO $ liftM2 (:) m go

push :: IORef [a] -> a -> IO ()
push r x = do xs <- readIORef r; writeIORef r $ x : xs

pop :: IORef [a] -> IO a
pop r = do x : xs <- readIORef r; writeIORef r xs; return x

type Wrapper a = a -> IO (FunPtr a)

manage :: IO (Ptr a) -> (Ptr a -> IO ()) -> IO () -> IO (ForeignPtr a)
manage new free finalize = do p <- new; Foreign.Concurrent.newForeignPtr p $ free p >> finalize

buffer :: Storable a => Int -> (Ptr a -> IO ()) -> IO [a]
buffer n action = allocaArray n $ \ p -> do action p; peekArray n p
