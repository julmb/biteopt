module Foreign.Utilities (Wrapper, withWrapper) where

import Control.Monad.Cont
import Foreign

type Wrapper a = a -> IO (FunPtr a)

withWrapper :: Wrapper a -> a -> ContT r IO (FunPtr a)
withWrapper wrapper f = ContT $ \ action -> do
    p <- wrapper f
    result <- action p
    freeHaskellFunPtr p
    return result
