module Foreign.Utilities (inf, Wrapper, withWrapper) where

import Control.Exception
import Control.Monad.Cont
import System.IO.Unsafe
import Foreign

inf :: IO a -> IO [a]
inf m = go where go = unsafeInterleaveIO $ liftM2 (:) m go

type Wrapper a = a -> IO (FunPtr a)

withWrapper :: IO (FunPtr a) -> ContT r IO (FunPtr a)
withWrapper = lift
